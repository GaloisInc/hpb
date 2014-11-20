{
{-# LANGUAGE OverloadedStrings #-}
module Data.HPB.Parser
  ( parseDecls
  ) where

import Control.Applicative
import qualified Data.ByteString.Lazy as LazyBS
import Data.Text (Text)
import qualified Data.Text as Text
import Text.PrettyPrint.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.Leijen as PP

import Data.HPB.AST
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

  'enum'       { Posd (TKeyword "enum")       _ }
  'extend'     { Posd (TKeyword "extend")     _ }
  'extensions' { Posd (TKeyword "extensions") _ }
  'false'      { Posd (TKeyword "false")      _ }
  'import'     { Posd (TKeyword "import")   _ }
  'max'        { Posd (TKeyword "max")      _ }
  'message'    { Posd (TKeyword "message")  _ }
  'oneof'      { Posd (TKeyword "oneof")    _ }
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
  'true'       { Posd (TKeyword "true")     _ }
%%


topLevel :: { Package }
topLevel : 'package' compound_name ';' list(decl) { Package (Just $2) $4 }
         | list(decl) { Package Nothing $1 }

decl :: { Decl }
decl : 'import' importvis string ';' { Import $2 $3 }
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

option_name :: { Posd OptionName }
option_name : ident { KnownName `fmap` $1 }
            | custom_option list(option_deref) { (`CustomName` $2) `fmap` $1 }

custom_option :: { Posd CustomOption }
custom_option : custom_option_tkn { tknAsCustomOption `fmap` $1 }

option_deref :: { Posd Ident }
option_deref : '.' ident { $2 }

enum_decl :: { EnumDecl }
enum_decl : 'enum' ident '{' list(enum_value) '}' { EnumDecl $2 $4 }

enum_value :: { EnumField }
enum_value
  : option_decl { EnumOption $1 }
  | ident '=' num ';' { EnumValue $1 (val $3) }

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
  | 'extensions' num 'to' extension_upper ';' { Extensions (pos $1) $2 $4 }
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

field_type :: { Posd FieldType }
field_type : scalar        { ScalarFieldType `fmap` $1 }
           | compound_name { Posd (NamedFieldType $1) (compoundNamePos $1) }
           | '.' compound_name { Posd (GlobalNamedType $2) (pos $1) }

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

type_list1 :: { [Posd FieldType] }
type_list1 : '(' comma_sep_list1(field_type) ')' { $2 }

rpc_end :: { [OptionDecl] }
rpc_end : ';' { [] }
        | '{' list(option_decl) '}' { $2 }


val :: { Posd Val }
val : ident  { IdentVal `fmap` $1 }
    | num    { NumVal `fmap` $1 }
    | string { StringVal `fmap` $1 }
    | 'true'  { Posd (BoolVal True)  (pos $1) }
    | 'false' { Posd (BoolVal False) (pos $1) }

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
-- Parsing declarations

parseDecls :: FilePath -> LazyBS.ByteString -> Either String Package
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
    TIdent t -> t
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
