{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.HPB.AST
  ( Package(..)
  , Decl(..)
  , CompoundName(..)
  , compoundNamePos
  , ImportVis(..)
  , ppDecls
    -- * Messages
  , MessageDecl(..)
  , ExtendDecl(..)
  , MessageField(..)
  , FieldDecl(..)
  , FieldRule(..)
  , Field(..)
  , findFieldOption
  , fieldDefault
  , fieldIsPacked
  , FieldType(..)
  , extensionMax
    -- * Enum
  , EnumDecl(..)
  , enumPos
  , EnumField(..)
    -- * Services
  , ServiceDecl(..)
  , ServiceField(..)
  , RpcMethod(..)
    -- * Options
  , OptionDecl(..)
  , OptionName(..)
  , Val(..)
  , asBoolVal
    -- * Base types
  , Ident(..)
  , ScalarType(..)
  , NumLit(..)
  , Base(..)
  , baseVal
  , StringLit(..)
  , CustomOption(..)
    -- * Source position information.
  , SourcePos(..)
  , nextCol
  , nextLine
  , Posd(..)
  , failAt
  ) where

import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>), line)

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail( MonadFail )
#endif

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable)
import Data.Functor ((<$>))
import Data.Traversable (Traversable)
#endif

failAt :: MonadFail m => SourcePos -> String -> m a
failAt p msg = fail $ show $
  pretty p <> text ":" <$$>
  indent 2 (text msg)

------------------------------------------------------------------------
-- SourcePos

data SourcePos = Pos { filename :: !Text
                     , line :: !Int
                     , col :: !Int
                     }

instance Pretty SourcePos where
  pretty p = text (Text.unpack (filename p)) <> text ":"
           <> int (line p) <> text ":"
           <> int (col p)

instance Show SourcePos where
  show p = show (pretty p)

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
                   } deriving (Functor, Foldable, Traversable, Show)

------------------------------------------------------------------------
-- Ident

newtype Ident = Ident { identText :: Text }
  deriving (Eq, Ord)

instance IsString Ident where
  fromString = Ident . fromString

instance Pretty Ident where
  pretty (Ident t) = text (Text.unpack t)

instance Show Ident where
  show (Ident t) = Text.unpack t

------------------------------------------------------------------------
-- CompoundName

newtype CompoundName = CompoundName [Posd Ident]

instance Pretty CompoundName where
  pretty (CompoundName nms) = hcat (punctuate dot (pretty . val <$> nms))

compoundNamePos :: CompoundName -> SourcePos
compoundNamePos (CompoundName (h:_)) = pos h
compoundNamePos _ = error "internal: empty compount name."

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
  deriving (Eq)

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
-- Base

data Base = Oct | Dec | Hex

baseVal :: Base -> Integer
baseVal Oct =  8
baseVal Dec = 10
baseVal Hex = 16

------------------------------------------------------------------------
-- NumLit

data NumLit = NumLit { numBase :: Base
                     , numVal :: Integer
                     }

instance Eq NumLit where
  x == y = numVal x == numVal y

instance Ord NumLit where
  compare x y = compare (numVal x) (numVal y)

instance Num NumLit where
  x + y = x { numVal = numVal x + numVal y }
  x * y = x { numVal = numVal x * numVal y }
  negate n = n { numVal = negate (numVal n) }
  abs x = x { numVal = abs (numVal x) }
  signum x = signum x
  fromInteger = NumLit Dec


instance Show NumLit where
  show n = show (pretty n)

instance Pretty NumLit where
  pretty (NumLit b v) = do
    -- Get prefix for base
    let prefix = case b of
                   Oct -> text "0"
                   Dec -> text ""
                   Hex -> text "0x"
    -- Prefix negation sign if needed.
    case v of
      0 -> prefix <> text "0"
      _ | v < 0 -> text "-" <> prefix <> ppDigits (abs v) PP.empty
        | otherwise         -> prefix <> ppDigits v PP.empty
        where -- Pretty print digits
              ppDigits 0 prev = prev
              ppDigits n prev = do
                let (q,r) = n `quotRem` baseVal b
                ppDigits q (integer r <> prev)

------------------------------------------------------------------------
-- StringLit

newtype StringLit = StringLit { stringText :: Text }

instance Show StringLit where
  show l = show (pretty l)

instance Pretty StringLit where
  pretty (StringLit t) = dquotes (hcat (go <$> Text.unpack t))
    where go '\\' = text "\\\\"
          go '\"' = text "\\\""
          go c = char c

------------------------------------------------------------------------
-- CustomOption

newtype CustomOption = CustomOption Text

instance Pretty CustomOption where
  pretty (CustomOption t) = parens (text (Text.unpack t))

------------------------------------------------------------------------
-- Val

data Val
   = NumVal    NumLit
   | IdentVal  Ident
   | StringVal StringLit
   | BoolVal   Bool

instance Pretty Val where
  pretty (NumVal v) = pretty v
  pretty (IdentVal v) = pretty v
  pretty (StringVal v) = pretty v
  pretty (BoolVal b) = pretty b

asBoolVal :: MonadFail m => String -> Posd Val -> m Bool
asBoolVal msg (Posd v p) =
  case v of
    BoolVal b -> return b
    _ -> failAt p msg

------------------------------------------------------------------------
-- OptionDecl

data OptionDecl = OptionDecl !(Posd OptionName) !(Posd Val)

instance Pretty OptionDecl where
  pretty (OptionDecl nm v) =
    text "option" <+> pretty (val nm) <+> text "=" <+> pretty (val v) <> text ";"

data OptionName = KnownName !Ident
                | CustomName !CustomOption ![Posd Ident]

instance IsString OptionName where
  fromString nm = KnownName (fromString nm)

instance Pretty OptionName where
  pretty (KnownName o) = pretty o
  pretty (CustomName o l) = pretty o <> hsep ((\f -> text "." <> pretty (val f)) <$> l)

------------------------------------------------------------------------
-- EnumField

data EnumField
   = EnumValue (Posd Ident) NumLit
   | EnumOption OptionDecl

instance Pretty EnumField where
  pretty (EnumValue nm l) = pretty (val nm) <+> text "=" <+> pretty l <> text ";"
  pretty (EnumOption d) = pretty d

------------------------------------------------------------------------
-- EnumDecl

data EnumDecl = EnumDecl { enumIdent :: !(Posd Ident)
                         , enumFields :: !([EnumField])
                         }

enumPos :: EnumDecl -> SourcePos
enumPos = pos . enumIdent

instance Pretty EnumDecl where
  pretty (EnumDecl nm opts) =
    text "enum" <+> pretty (val nm) <+> text "{" <$$>
    indent 2 (vcat (pretty <$> opts)) <$$>
    text "}"

------------------------------------------------------------------------
-- FieldType

data FieldType = ScalarFieldType ScalarType
               | NamedFieldType  CompoundName
               | GlobalNamedType CompoundName

instance Pretty FieldType where
  pretty (ScalarFieldType tp) = pretty tp
  pretty (NamedFieldType nm) = pretty nm
  pretty (GlobalNamedType nm) = text "." <> pretty nm

------------------------------------------------------------------------
-- Field

data Field = Field { fieldType :: !(Posd FieldType)
                   , fieldName :: !(Posd Ident)
                     -- | The unique numbered tag associated with the fiel.d
                   , fieldTag  :: !(Posd NumLit)
                   , fieldOptions :: [OptionDecl]
                   }

findFieldOption :: Ident -> Field -> Maybe (Posd Val)
findFieldOption nm f = listToMaybe $
  [ v | OptionDecl (Posd (KnownName onm) _) v <- fieldOptions f
      , onm == nm
  ]

fieldDefault :: Field -> Maybe (Posd Val)
fieldDefault = findFieldOption "default"

fieldIsPacked :: MonadFail m => Field -> m Bool
fieldIsPacked f =
  case findFieldOption "packed" f of
    Just v -> asBoolVal "packed value must be a Boolean." v
    Nothing -> return False

instance Pretty Field where
  pretty (Field tp nm v opts) =
    pretty (val tp)
      <+> pretty (val nm) <+> text "=" <+> pretty (val v)
      <> ppInlineOptions opts <> text ";"

ppInlineOptions :: [OptionDecl] -> Doc
ppInlineOptions [] = PP.empty
ppInlineOptions l = text " [" <> hsep (punctuate comma (pretty <$> l)) <> text "]"

------------------------------------------------------------------------
-- FieldRule

data FieldRule = Required | Optional | Repeated

instance Pretty FieldRule where
  pretty Required = text "required"
  pretty Optional = text "optional"
  pretty Repeated = text "repeated"

------------------------------------------------------------------------
-- FieldDecl

-- | A pair contining a field and the associated rule.
data FieldDecl = FieldDecl { fieldDeclRule :: FieldRule
                           , fieldDeclField :: Field
                           }

instance Pretty FieldDecl where
  pretty (FieldDecl rl f) = pretty rl <+> pretty f

------------------------------------------------------------------------
-- ExtendDecl

data ExtendDecl = ExtendDecl { extendMessage :: Posd Ident
                             , extendFields :: [FieldDecl]
                             }

instance Pretty ExtendDecl where
  pretty (ExtendDecl nm fields) =
    text "extend" <+> pretty (val nm) <+> text "{" <$$>
    indent 2 (vcat (pretty <$> fields)) <$$>
    text "}"

------------------------------------------------------------------------
-- MessageDecl

extensionMax :: NumLit
extensionMax = NumLit Dec (2^(29::Int) - 1)

data MessageDecl
   = MessageDecl { messageName :: !(Posd Ident)
                 , messageFields :: !([MessageField])
                 }

data MessageField
   = MessageField FieldDecl
   | MessageOption OptionDecl
   | OneOf (Posd Ident) [Field]
   | Extensions SourcePos (Posd NumLit) (Posd NumLit)
   | LocalEnum    EnumDecl
   | LocalMessage MessageDecl
   | LocalExtend  ExtendDecl

instance Pretty MessageDecl where
  pretty (MessageDecl nm fields) =
    text "message" <+> pretty (val nm) <+> text "{" <$$>
    indent 2 (vcat (pretty <$> fields)) <$$>
    text "}"

instance Pretty MessageField where
  pretty (MessageField f) = pretty f
  pretty (MessageOption o) = pretty o
  pretty (OneOf nm fields) =
    text "oneof" <+> pretty (val nm) <+> text "{" <$$>
    indent 2 (vcat (pretty <$> fields)) <$$>
    text "}"
  pretty (Extensions _ l h) =
    text "extensions" <+> pretty (val l) <+> text "to" <+> pretty (val h)
  pretty (LocalEnum d)   = pretty d
  pretty (LocalMessage d) = pretty d
  pretty (LocalExtend d) = pretty d


------------------------------------------------------------------------
-- ServiceDecl

data ServiceDecl = ServiceDecl !(Posd Ident) !([ServiceField])

instance Pretty ServiceDecl where
  pretty (ServiceDecl nm fields) =
    text "service" <+> pretty (val nm) <+> text "{" <$$>
    indent 2 (vcat (pretty <$> fields)) <$$>
    text "}"

data ServiceField
   = ServiceOption !OptionDecl
   | ServiceRpcMethod !RpcMethod

instance Pretty ServiceField where
  pretty (ServiceOption d) = pretty d
  pretty (ServiceRpcMethod m) = pretty m

data RpcMethod = RpcMethod { rpcName :: (Posd Ident)
                           , rpcInputs :: [Posd FieldType]
                           , rpcReturns :: [Posd FieldType]
                           , rpcOptions :: [OptionDecl]
                           }

ppTypeList :: [Posd FieldType] -> Doc
ppTypeList tps = parens (hsep (punctuate comma (pretty.val <$> tps)))

ppRpcOptions :: [OptionDecl] -> Doc
ppRpcOptions [] = text ";"
ppRpcOptions l = text " {" <$$>
  indent 2 (vcat (pretty <$> l)) <$$>
  text "}"

instance Pretty RpcMethod where
  pretty m =
    text "rpc" <+> pretty (val (rpcName m)) <> ppTypeList (rpcInputs m)
               <+> text "returns" <+> ppTypeList (rpcReturns m)
               <> ppRpcOptions (rpcOptions m)

------------------------------------------------------------------------
-- Decl

data ImportVis = Public | Private

data Decl
   = Import ImportVis (Posd StringLit)
   | Option OptionDecl
   | Enum EnumDecl
   | Message MessageDecl
   | Extend ExtendDecl
   | Service ServiceDecl

instance Pretty Decl where
  pretty decl =
    case decl of
      Import v nm -> text "import" <> visd <+> pretty (val nm) <> text ";"
        where visd = case v of
                       Public -> text " public"
                       Private -> PP.empty
      Option d -> pretty d
      Enum d -> pretty d
      Message m -> pretty m
      Extend d -> pretty d
      Service d -> pretty d

ppDecls :: [Decl] -> Doc
ppDecls decls = vcat (pretty <$> decls)

------------------------------------------------------------------------
-- Package

data Package = Package (Maybe CompoundName) [Decl]

instance Pretty Package where
  pretty (Package (Just nm) decls) =
    text "package" <+> pretty nm <> text ";" <$$>
    vcat (pretty <$> decls)
  pretty (Package Nothing decls) =
    vcat (pretty <$> decls)
