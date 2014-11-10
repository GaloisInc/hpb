{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.HPB.Lexer
  ( Base(..)
  , NumLit(..)
  , StringLit(..)
  , ScalarType(..)
  , CustomOption(..)
  , Token(..)
  , Posd(..)
  , SourcePos(..)
  , Alex
  , runAlex
  , getSourcePos
  , lexToken
  , lexer
  , tokenizeFile
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Error
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Char
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import Data.Word
import Text.PrettyPrint.Leijen as PP hiding (line)
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

@special = "(" | ")" | "[" | "]" | "{" | "}" | "=" | "." | "," | ";"

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
@idname { TIdent }

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
-- Base

data Base = Oct | Dec | Hex

baseVal :: Base -> Integer
baseVal Oct =  8
baseVal Dec = 10
baseVal Hex = 16

------------------------------------------------------------------------
-- NumLit

data NumLit = NumLit Base Integer

instance Show NumLit where
  show n = show (pretty n)

instance Pretty NumLit where
  pretty (NumLit b v) = do
    let ppNum 0 = text "0"
        ppNum n = ppDigits n PP.empty
        ppDigits 0 prev = prev
        ppDigits n prev = do
          let (q,r) = n `quotRem` baseVal b
          ppDigits q (integer r <> prev)
    case b of
      Oct -> text "0" <> ppNum v
      Dec -> ppNum v
      Hex -> text "0x" <> ppNum v

------------------------------------------------------------------------
-- StringLit

newtype StringLit = StringLit Text

instance Show StringLit where
  show l = show (pretty l)

instance Pretty StringLit where
  pretty (StringLit t) = text "\"" <> Text.foldr go (text "\"") t
    where go '\\' s = text "\\\\" <> s
          go '\"' s = text "\\\"" <> s
          go c s = char c <> s

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
-- ScalarType

data ScalarType
   = DoubleType
   | FloatType
   | Int32Type
   | Int64Type
   | Uint32Type
   | Uint64Type
   | Sint32Type
   | Sint64Type
   | Fixed32Type
   | Fixed64Type
   | Sfixed32Type
   | Sfixed64Type
   | BoolType
   | StringType
   | BytesType


instance Show ScalarType where
  show tp =
    case tp of
      DoubleType   -> "double"
      FloatType    -> "float"
      Int32Type    -> "int32"
      Int64Type    -> "int64"
      Uint32Type   -> "uint32"
      Uint64Type   -> "uint64"
      Sint32Type   -> "sint32"
      Sint64Type   -> "sint64"
      Fixed32Type  -> "fixed32"
      Fixed64Type  -> "fixed64"
      Sfixed32Type -> "sfixed32"
      Sfixed64Type -> "sfixed64"
      BoolType     -> "bool"
      StringType   -> "string"
      BytesType    -> "bytes"

instance Pretty ScalarType where
  pretty tp = text (show tp)

------------------------------------------------------------------------
-- CustomOption

newtype CustomOption = CustomOption Text

instance Pretty CustomOption where
  pretty (CustomOption t) = parens (text (Text.unpack t))

------------------------------------------------------------------------
-- Token

data Token
     -- | The base of an number and its value.
   = TNum NumLit
   | TIdent Text
     -- | The name of an extension.
   | TString StringLit
   | TScalar ScalarType
   | TCustomOption CustomOption
   | TKeyword Text
   | TSpecial Text
   | TEOF

instance Show Token where
  show (TNum l) = show l
  show (TIdent t) = Text.unpack t
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
-- SourcePos

data SourcePos = Pos { filename :: !Text
                     , line :: !Int
                     , col :: !Int
                     } deriving Show

nextCol :: SourcePos -> SourcePos
nextCol p = p { col = col p + 1 }

nextLine :: SourcePos -> SourcePos
nextLine p = p { line = line p + 1
               , col = 0
               }

------------------------------------------------------------------------
-- Posd

data Posd v = Posd { val :: !v
                   , pos :: !SourcePos
                   } deriving (Functor, Show)

------------------------------------------------------------------------
-- AlexInput

data AlexInput = AlexInput {
      _alex_bs   :: LazyBS.ByteString -- the remaining input.
    , _alex_pos  :: {-# UNPACK #-} !SourcePos
    , _bytes_to_char :: {-# UNPACK #-} !Int64
    , _prev_char :: {-# UNPACK #-} !Char
    }

alex_bs :: Simple Lens AlexInput LazyBS.ByteString
alex_bs = lens _alex_bs (\s v -> s { _alex_bs = v })

-- | Current position.
alex_pos :: Simple Lens AlexInput SourcePos
alex_pos = lens _alex_pos (\s v -> s { _alex_pos = v })

-- | Number of bytes remaining in current char.
bytes_to_char :: Simple Lens AlexInput Int64
bytes_to_char = lens _bytes_to_char (\s v -> s { _bytes_to_char = v })

-- | Number of bytes remaining in current char.
prev_char :: Simple Lens AlexInput Char
prev_char = lens _prev_char (\s v -> s { _prev_char = v })

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

newtype Alex a = Alex { unAlex :: StateT AlexInput (ErrorT String Identity) a }
  deriving (Functor, Applicative, Monad)

runAlex :: FilePath
        -> LazyBS.ByteString
        -> Alex a
        -> Either String a
runAlex path bs m = runIdentity $ runErrorT $ evalStateT (unAlex m) s
  where s = AlexInput { _alex_bs = bs
                      , _alex_pos = Pos (Text.pack path) 1 1
                      , _bytes_to_char = 0
                      , _prev_char = '\n'
                      }

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
    Left e -> fail e
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
      Alex $ put inp'
      -- Length is difference in bytestring lengths
      let len = bytes_remaining inp - bytes_remaining inp'
      -- Get text from string.
      let s = decodeUtf8 $ LazyBS.toStrict $ LazyBS.take (fromIntegral len) (inp^.alex_bs)
      let p = inp^.alex_pos
      return $ Posd (action s) p

getSourcePos :: Alex SourcePos
getSourcePos = Alex $ use alex_pos

lexer :: (Posd Token -> Alex a) -> Alex a
lexer = (lexToken >>=)

_unused :: a
_unused = undefined iUnbox
}
