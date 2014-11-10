{
{-# LANGUAGE OverloadedStrings #-}
module Data.HPB.Parser
  ( Decl(..)
  , ImportVis(..)
  , CompoundName(..)
  , Ident(..)
  , Val(..)
  , EnumDecl(..)
  , MessageDecl(..)
  , ExtendDecl(..)
  , ppDecls
  , parseDecls
  ) where

import Control.Applicative
import qualified Data.ByteString.Lazy as LazyBS
import Data.Text (Text)
import qualified Data.Text as Text
import Text.PrettyPrint.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.Leijen as PP

import Data.HPB.Lexer
}

%name parse topLevel
%error { parseError }
%tokentype { Posd Token }
%monad { Alex } { (>>=) } { return }
%lexer { lexer } { Posd TEOF _ }

%token
  num_tkn      { Posd (TNum _)   _ }
  ident_tkn    { Posd (TIdent _) _ }
  string_tkn   { Posd (TString _) _ }
  scalar_tkn   { Posd (TScalar _) _ }
  custom_option_tkn { Posd (TCustomOption _) _ }
  '('          { Posd (TSpecial "(") _ }
  ')'          { Posd (TSpecial ")") _ }
  '['          { Posd (TSpecial "[") _ }
  ']'          { Posd (TSpecial "]") _ }
  '{'          { Posd (TSpecial "{") _ }
  '}'          { Posd (TSpecial "}") _ }
  '='          { Posd (TSpecial "=") _ }
  '.'          { Posd (TSpecial ".") _ }
  ','          { Posd (TSpecial ",") _ }
  ';'          { Posd (TSpecial ";") _ }

  'enum'       { Posd (TKeyword "enum")    _ }
  'extend'     { Posd (TKeyword "extend")  _ }
  'extensions' { Posd (TKeyword "extensions") _ }
  'import'     { Posd (TKeyword "import")  _ }
  'max'        { Posd (TKeyword "max")      _ }
  'message'    { Posd (TKeyword "message")  _ }
  'oneof'      { Posd (TKeyword "oneof")   _ }
  'option'     { Posd (TKeyword "option")   _ }
  'optional'   { Posd (TKeyword "optional") _ }
  'package'    { Posd (TKeyword "package")  _ }
  'public'     { Posd (TKeyword "public")   _ }
  'repeated'   { Posd (TKeyword "repeated") _ }
  'required'   { Posd (TKeyword "required") _ }
  'returns'    { Posd (TKeyword "returns")  _ }
  'rpc'        { Posd (TKeyword "rpc")      _ }
  'service'    { Posd (TKeyword "service")  _ }
  'to'         { Posd (TKeyword "to")       _ }
%%


topLevel :: { [Decl] }
topLevel : list(decl) { $1 }

decl :: { Decl }
decl : 'package' compound_name ';' { PackageName $2 }
     | 'import' importvis string ';' { Import $2 $3 }
     | option_decl  { Option $1 }
     | enum_decl    { Enum $1 }
     | extend_decl  { Extend $1 }
     | message_decl { Message $1 }
     | service_decl { Service $1 }

importvis :: { ImportVis }
importvis : { Private }
          | 'public' { Public }

inline_option :: { OptionDecl }
inline_option : option_name '=' val { OptionDecl $1 $3 }

option_decl :: { OptionDecl }
option_decl : 'option' inline_option ';' { $2 }

option_name :: { OptionName }
option_name : ident { KnownName $1 }
            | custom_option list(option_deref) { CustomName $1 $2 }

custom_option :: { Posd CustomOption }
custom_option : custom_option_tkn { tknAsCustomOption `fmap` $1 }

option_deref :: { Posd Ident }
option_deref : '.' ident { $2 }

enum_decl :: { EnumDecl }
enum_decl : 'enum' ident '{' list(enum_value) '}' { EnumDecl $2 $4 }

enum_value :: { EnumValue }
enum_value : ident '=' num ';' { EnumValue $1 $3 }

extend_decl :: { ExtendDecl }
extend_decl : 'extend' ident '{' list(field_decl) '}' { ExtendDecl $2 $4 }

message_decl :: { MessageDecl }
message_decl : 'message' ident '{' list(message_field) '}' { MessageDecl $2 $4 }

field :: { Field }
field : field_type ident '=' num options ';'
        { Field $1 $2 $4 $5 }

message_field :: { MessageField }
message_field
  : field_decl   { MessageField $1 }
  | option_decl  { MessageOption $1 }
  | 'oneof' ident '{' list(field) '}' { OneOf $2 $4 }
  | 'extensions' num 'to' extension_upper { Extensions $2 $4 }
  | enum_decl    { LocalEnum $1 }
  | message_decl { LocalMessage $1 }
  | extend_decl  { LocalExtend $1 }

field_decl :: { FieldDecl }
field_decl : field_rule field { FieldDecl $1 $2 }

extension_upper :: { Posd NumLit }
extension_upper : num { $1 }
                | 'max' { Posd extensionMax (pos $1) }

field_rule :: { FieldRule }
field_rule : 'required' { Required }
           | 'optional' { Optional }
           | 'repeated' { Repeated }

field_type :: { FieldType }
field_type : scalar        { ScalarFieldType  (val $1) }
           | compound_name { MessageFieldType $1 }

options :: { [OptionDecl] }
options : { [] }
        | '[' comma_sep_list(inline_option) ']' { $2 }

service_decl :: { ServiceDecl }
service_decl
  : 'service' ident '{' list(service_field) '}' { ServiceDecl $2 $4 }

service_field :: { ServiceField }
service_field
  : option_decl { ServiceOption $1 }
  | rpc_method_decl { ServiceRpcMethod $1 }

rpc_method_decl :: { RpcMethod }
rpc_method_decl
  : 'rpc' ident type_list1 'returns' type_list1 rpc_end
    { RpcMethod $2 $3 $5 $6 }

type_list1 :: { [FieldType] }
type_list1 : '(' comma_sep_list1(field_type) ')' { $2 }

rpc_end :: { [OptionDecl] }
rpc_end : ';' { [] }
        | '{' list(option_decl) '}' { $2 }


val :: { Posd Val }
val : ident  { IdentVal `fmap` $1 }
    | num    { NumVal `fmap` $1 }
    | string { StringVal `fmap` $1 }

compound_name :: { CompoundName }
compound_name : compound_name_rev { CompoundName (reverse $1) }

compound_name_rev :: { [Posd Ident] }
compound_name_rev : ident { [$1] }
                  | compound_name_rev '.' ident { $3 : $1 }


ident :: { Posd Ident }
ident : ident_tkn { tknAsIdent `fmap` $1 }

num :: { Posd NumLit }
num : num_tkn { tknAsNum `fmap` $1 }

string :: { Posd StringLit }
string : string_tkn { tknAsString `fmap` $1 }

scalar :: { Posd ScalarType }
scalar : scalar_tkn { tknAsScalar `fmap` $1 }

comma_sep_list(e) : { [] }
                  | comma_sep_list1(e) { $1 }

comma_sep_list1(e) : comma_sep_revlist1(e) { reverse $1 }

comma_sep_revlist1(e) : e { [$1] }
                      | comma_sep_revlist1(e) ',' e { $3 : $1 }

list(e) : list_rev(e) { reverse $1 }

list_rev(e) : { [] }
            | list_rev(e) e { $2 : $1 }

{

------------------------------------------------------------------------
-- Ident

newtype Ident = Ident Text

instance Pretty Ident where
  pretty (Ident nm) = text (Text.unpack nm)

------------------------------------------------------------------------
-- CompoundName

newtype CompoundName = CompoundName [Posd Ident]

instance Pretty CompoundName where
  pretty (CompoundName nms) = hcat (punctuate dot (pretty . val <$> nms))

------------------------------------------------------------------------
-- Val

data Val
   = NumVal    NumLit
   | IdentVal  Ident
   | StringVal StringLit

instance Pretty Val where
  pretty (NumVal v) = pretty v
  pretty (IdentVal v) = pretty v
  pretty (StringVal v) = pretty v

------------------------------------------------------------------------
-- EnumValue

data EnumValue = EnumValue (Posd Ident) (Posd NumLit)

instance Pretty EnumValue where
  pretty (EnumValue nm l) = pretty (val nm) <+> text "=" <+> pretty (val l) <> text ";"

------------------------------------------------------------------------
-- EnumDecl

data EnumDecl = EnumDecl (Posd Ident) [EnumValue]

instance Pretty EnumDecl where
  pretty (EnumDecl nm opts) =
    text "enum" <+> pretty (val nm) <+> text "{" <$$>
    indent 2 (vcat (pretty <$> opts)) <$$>
    text "}"

------------------------------------------------------------------------
-- FieldRule

data FieldRule = Required | Optional | Repeated

instance Pretty FieldRule where
  pretty Required = text "required"
  pretty Optional = text "optional"
  pretty Repeated = text "repeated"

------------------------------------------------------------------------
-- FieldType

data FieldType = ScalarFieldType ScalarType
               | MessageFieldType CompoundName

instance Pretty FieldType where
  pretty (ScalarFieldType tp) = pretty tp
  pretty (MessageFieldType nm) = pretty nm

------------------------------------------------------------------------
-- Field

data Field = Field  FieldType (Posd Ident) (Posd NumLit) [OptionDecl]

instance Pretty Field where
  pretty (Field tp nm v opts) =
    pretty tp <+> pretty (val nm) <+> text "=" <+> pretty (val v)
              <> ppInlineOptions opts <> text ";"

ppInlineOptions :: [OptionDecl] -> Doc
ppInlineOptions [] = PP.empty
ppInlineOptions l = text " [" <> hsep (punctuate comma (pretty <$> l)) <> text "]"

------------------------------------------------------------------------
-- FieldDecl

data FieldDecl = FieldDecl FieldRule Field

instance Pretty FieldDecl where
  pretty (FieldDecl rl f) = pretty rl <+> pretty f

------------------------------------------------------------------------
-- ExtendDecl

data ExtendDecl = ExtendDecl (Posd Ident) [FieldDecl]

instance Pretty ExtendDecl where
  pretty (ExtendDecl nm fields) =
    text "extend" <+> pretty (val nm) <+> text "{" <$$>
    indent 2 (vcat (pretty <$> fields)) <$$>
    text "}"

------------------------------------------------------------------------
-- OptionDecl

data OptionDecl = OptionDecl !OptionName !(Posd Val)

instance Pretty OptionDecl where
  pretty (OptionDecl nm v) =
    text "option" <+> pretty nm <+> text "=" <+> pretty (val v) <> text ";"


data OptionName = KnownName !(Posd Ident)
                | CustomName !(Posd CustomOption) ![Posd Ident]

instance Pretty OptionName where
  pretty (KnownName o) = pretty (val o)
  pretty (CustomName o l) = pretty (val o) <> hsep ((\f -> text "." <> pretty (val f)) <$> l)

------------------------------------------------------------------------
-- MessageDecl

extensionMax :: NumLit
extensionMax = NumLit Dec (2^(29::Int) - 1)

data MessageDecl = MessageDecl (Posd Ident) [MessageField]

data MessageField
   = MessageField FieldDecl
   | MessageOption OptionDecl
   | OneOf (Posd Ident) [Field]
   | Extensions (Posd NumLit) (Posd NumLit)
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
  pretty (Extensions l h) =
    text "extensions" <+> pretty (val l) <+> text "to" <+> pretty (val h)
  pretty (LocalEnum d)   = pretty d
  pretty (LocalMessage d) = pretty d

------------------------------------------------------------------------
-- ServiceDecl

data ServiceDecl = ServiceDecl !(Posd Ident) !([ServiceField])

data ServiceField
   = ServiceOption !OptionDecl
   | ServiceRpcMethod !RpcMethod

data RpcMethod = RpcMethod { rpcName :: (Posd Ident)
                           , rpcInputs :: [FieldType]
                           , rpcReturns :: [FieldType]
                           , rpcOptions :: [OptionDecl]
                           }

------------------------------------------------------------------------
-- Decl

data ImportVis = Public | Private

data Decl
   = PackageName CompoundName
   | Import ImportVis (Posd StringLit)
   | Option OptionDecl
   | Enum EnumDecl
   | Message MessageDecl
   | Extend ExtendDecl
   | Service ServiceDecl

instance Pretty Decl where
  pretty d =
    case d of
      PackageName nm -> text "package" <+> pretty nm <> text ";"
      Import v nm -> text "import" <> visd <+> pretty (val nm)
        where visd = case v of
                       Public -> text " public"
                       Private -> PP.empty
      Option d -> pretty d
      Enum d -> pretty d
      Message m -> pretty m
      Extend d -> pretty d

ppDecls :: [Decl] -> Doc
ppDecls decls = vcat (pretty <$> decls)

------------------------------------------------------------------------
-- Parsing declarations

parseDecls :: FilePath -> LazyBS.ByteString -> Either String [Decl]
parseDecls path bs = runAlex path bs parse

parseError :: Posd Token -> Alex a
parseError tkn = do
  let p = pos tkn
  case val tkn of
    TEOF -> fail $ "Unexpected end of file at " ++ show p ++ "."
    TString s -> fail $ "Unexpected string " ++ show s
                        ++ " at " ++ show p ++ "."
    info -> do
      let tkn = show info
          tp | length tkn == 1 = "char"
             | otherwise = "token"
      fail $ "Unexpected " ++ tp ++ " '" ++ show info ++ "' at " ++ show p ++ "."

------------------------------------------------------------------------
-- Token utilities

tknAsIdent :: Token -> Ident
tknAsIdent tkn =
  case tkn of
    TIdent t -> Ident t
    _ -> error $ "internal: Token is not an identifier."

tknAsNum :: Token -> NumLit
tknAsNum tkn =
  case tkn of
    TNum n -> n
    _ -> error $ "internal: Token is not a num."

tknAsString :: Token -> StringLit
tknAsString tkn =
  case tkn of
    TString x -> x
    _ -> error $ "internal: Token is not a string."

tknAsScalar :: Token -> ScalarType
tknAsScalar tkn =
  case tkn of
    TScalar x -> x
    _ -> error $ "internal: Token is not a scalar type."

tknAsCustomOption :: Token -> CustomOption
tknAsCustomOption tkn =
  case tkn of
    TCustomOption x -> x
    _ -> error $ "internal: Token is not a scalar type."

}
