{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.HPB.Lexer
  ( Token(..)
  , Alex
  , runAlex
  , getSourcePos
  , lexToken
  , lexer
  , tokenizeFile
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Char
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import Data.Word
import Text.PrettyPrint.ANSI.Leijen as PP hiding (line)
import Data.HPB.AST
import Data.HPB.Partial

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail( MonadFail )
#endif

}

$alpha     = [a-z A-Z]
$digit     = 0-9
$idchar    = [$alpha $digit \_]
$newline = [\n\r\f\v\x0085\x2028\x2029]
$any = [. $newline]

@keyword
  = "enum"
  | "extend"
  | "extensions"
  | "false"
  | "import"
  | "max"
  | "message"
  | "oneof"
  | "option"
  | "optional"
  | "package"
  | "public"
  | "repeated"
  | "required"
  | "returns"
  | "rpc"
  | "service"
  | "to"
  | "true"

@special = "(" | ")" | "[" | "]" | "{" | "}" | "=" | "." | "," | ";" | "-"

@newline = $newline|"\r\n"
@string_char = [^$newline\\\"]|"\\\""

@idname = $alpha($idchar)*

protocTokens :-

@keyword { TKeyword }
@special { TSpecial }
"0"([0-7])+          { mkNum Oct . Text.drop 1 }
"0x"([0-9 a-f A-F])* { mkNum Hex . Text.drop 2 }
"0"|([0-9]+)         { mkNum Dec }

"double"   { mkScalarType DoubleType }
"float"    { mkScalarType FloatType }
"int32"    { mkScalarType Int32Type }
"int64"    { mkScalarType Int64Type }
"uint32"   { mkScalarType Uint32Type }
"uint64"   { mkScalarType Uint64Type }
"sint32"   { mkScalarType Sint32Type }
"sint64"   { mkScalarType Sint64Type }
"fixed32"  { mkScalarType Fixed32Type }
"fixed64"  { mkScalarType Fixed64Type }
"sfixed32" { mkScalarType Sfixed32Type }
"sfixed64" { mkScalarType Sfixed64Type }
"bool"     { mkScalarType BoolType }
"string"   { mkScalarType StringType }
"bytes"    { mkScalarType BytesType }

-- Identifiers
@idname { TIdent . Ident }

"(" @idname ")" { TCustomOption . CustomOption . trimText 1 1 }

\"(@string_char)*\" { mkStringLit }

-- Drop whitespace
$white+;

-- Inline comment: Drop rest of line up to line.
"//"[^$newline]*;

{

nlByte :: Word8
nlByte = fromIntegral (fromEnum '\n')

------------------------------------------------------------------------
-- Parsing operations

trimText :: Int -> Int -> Text -> Text
trimText f e = Text.drop f . Text.dropEnd e

parseStringLit :: Text -> StringLit
parseStringLit txt0 = StringLit (Text.unfoldrN (Text.length txt0) go txt_trim)
  where txt_trim = trimText 1 1 txt0
        go txt = do
          (c,txt1) <- Text.uncons txt
          case c of
            '\\' -> do
               (d,txt2) <- Text.uncons txt1
               case d of
                 '\\' -> return ('\\', txt2)
                 '\"' -> return ('\"', txt2)
                 _ -> error "internal: Could not interpret string literal."
            _ -> return (c, txt1)

------------------------------------------------------------------------
-- Token

data Token
     -- | The base of an number and its value.
   = TNum NumLit
   | TIdent Ident
     -- | The name of an extension.
   | TString StringLit
   | TScalar ScalarType
   | TCustomOption CustomOption
   | TKeyword Text
   | TSpecial Text
   | TEOF

instance Show Token where
  show (TNum l) = show l
  show (TIdent t) = show t
  show (TCustomOption t) = show (pretty t)
  show (TString t) = show t
  show (TKeyword t) = Text.unpack t
  show (TSpecial t) = Text.unpack t
  show (TScalar t) = show t
  show TEOF = "<eof>"

mkNum :: Base -> Text -> Token
mkNum base txt = TNum (NumLit base (Text.foldl go 0 txt))
  where go v c = baseVal base * v + toInteger (digitToInt c)

mkStringLit :: Text -> Token
mkStringLit txt0 = TString (parseStringLit txt0)

mkScalarType :: ScalarType -> Text -> Token
mkScalarType tp _ = TScalar tp

------------------------------------------------------------------------
-- AlexInput

data AlexInput = AlexInput {
      _alex_bs   :: LazyBS.ByteString -- the remaining input.
    , _alex_pos  :: {-# UNPACK #-} !SourcePos
    , _bytes_to_char :: {-# UNPACK #-} !Int64
    , _prev_char :: {-# UNPACK #-} !Char
      -- | Position of last token returned.
    , _alex_last_token_pos :: {-# UNPACK #-} !SourcePos
    }

-- | Remaining bytes in input.
alex_bs :: Lens' AlexInput LazyBS.ByteString
alex_bs = lens _alex_bs (\s v -> s { _alex_bs = v })

-- | Current position in file.
alex_pos :: Lens' AlexInput SourcePos
alex_pos = lens _alex_pos (\s v -> s { _alex_pos = v })

-- | Number of bytes remaining in current char.
bytes_to_char :: Lens' AlexInput Int64
bytes_to_char = lens _bytes_to_char (\s v -> s { _bytes_to_char = v })

-- | Number of bytes remaining in current char.
prev_char :: Lens' AlexInput Char
prev_char = lens _prev_char (\s v -> s { _prev_char = v })

-- | Position of last token returned (used for error reporting).
alex_last_token_pos :: Lens' AlexInput SourcePos
alex_last_token_pos = lens _alex_last_token_pos (\s v -> s { _alex_last_token_pos= v })

bytes_remaining :: AlexInput -> Int64
bytes_remaining inp = LazyBS.length (inp^.alex_bs)

nextInputLine :: LazyBS.ByteString -> AlexInput -> AlexInput
nextInputLine r inp = inp & alex_bs .~ r
                          & alex_pos %~ nextLine
                          & bytes_to_char .~ 0

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte inp | inp^.bytes_to_char > 0 = do
  (b,r) <- LazyBS.uncons (inp^.alex_bs)
  let inp' = inp & alex_bs .~ r
                 & bytes_to_char -~ 1
  return (b, inp')
alexGetByte inp = do
  let bs = inp^.alex_bs
  (c,l) <- UTF8.decode bs
  let Just (b,r) = LazyBS.uncons bs
  case c of
    '\r' ->
      case UTF8.uncons r of
        -- Handle '\r\n'
        Just ('\n', r') -> do
          let inp' = inp & nextInputLine r'
                         & prev_char .~ '\n'
          return (nlByte, inp')
        -- Handle '\r'
        _ -> do
          let inp' = inp & nextInputLine r
                         & prev_char .~ '\n'
          return (nlByte, inp')
    '\n' -> do
      let inp' = inp & nextInputLine r
                     & prev_char .~ '\n'
      return (nlByte, inp')
    _ -> do
      let inp' = inp & alex_bs .~ r
                     & alex_pos %~ nextCol
                     & bytes_to_char .~ (l - 1)
                     & prev_char .~ c
      return (b, inp')

------------------------------------------------------------------------
-- Alex

-- | Monad for lexer.
newtype Alex a = Alex { unAlex :: PartialT (StateT AlexInput Identity) a }
  deriving (Functor, Applicative, Monad, MonadFail)

runAlex :: FilePath
        -> LazyBS.ByteString
        -> Alex a
        -> Either (SourcePos, String) a
runAlex path bs m = runIdentity $ do
  let p0 = Pos (Text.pack path) 1 0
  let s0 = AlexInput { _alex_bs = bs
                     , _alex_pos = p0
                     , _bytes_to_char = 0
                     , _prev_char = '\n'
                     , _alex_last_token_pos = p0
                     }
  flip evalStateT s0 $ do
    mv <- runPartialT (unAlex m)
    s <- get
    return $
      case mv of
        Left e  -> Left (s^.alex_last_token_pos, e)
        Right v -> Right v

getTokens :: Alex [Posd Token]
getTokens = go []
  where go l = do
          tkn <- lexToken
          case val tkn of
            TEOF -> return (reverse l)
            _ -> go (tkn:l)

-- | Return tokens in a file.
-- Used primarily for debugging the lexer.
tokenizeFile :: FilePath -> IO [Posd Token]
tokenizeFile path = do
  bs <- LazyBS.readFile path
  case runAlex path bs getTokens of
    Left (p,e) -> fail $ "Error parsing at " ++ show p ++ "\n  " ++ e
    Right v -> return v

lexToken :: Alex (Posd Token)
lexToken = do
  inp <- Alex get
  case alexScan inp 0 of
    AlexEOF -> do
      return $ Posd TEOF (inp^.alex_pos)
    AlexError inp' ->
      case alexGetByte inp' of
        Just (_,inp0) -> do
          let c = inp0^.prev_char
          let p = inp^.alex_pos
          let tp = case c of
                     '\n' -> "newline"
                     _ | isSpace c -> "whitespace"
                     _ | isControl c -> "control character"
                     _ -> "character " ++ show c
          fail $ "Unexpected " ++ tp ++ " at " ++ show p ++ "."
        Nothing -> do
          fail $ "Unexpected end of file in middle of token."
    AlexSkip inp' _ -> do
      Alex $ put inp'
      lexToken
    AlexToken inp' _len action -> do
      let p = inp^.alex_pos
      -- Update position to next state with token position for the token we return.
      Alex $ put (inp' & alex_last_token_pos .~ p)
      -- Length is difference in bytestring lengths
      let len = bytes_remaining inp - bytes_remaining inp'
      -- Get text from string.
      let s = decodeUtf8 $ LazyBS.toStrict $ LazyBS.take (fromIntegral len) (inp^.alex_bs)
      let tkn = Posd (action s) p
      return tkn

getSourcePos :: Alex SourcePos
getSourcePos = Alex $ use alex_pos

lexer :: (Posd Token -> Alex a) -> Alex a
lexer = (lexToken >>=)
}
