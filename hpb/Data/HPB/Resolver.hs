------------------------------------------------------------------------
-- |
-- Module           : Data.HPB.Resolver
-- Description      : Provides overrides for symbolic simulator
--                    specific operations.
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- This module provides symbolic simulator specific overrides.
------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Data.HPB.Resolver
  ( Package(..)
  , ModuleName(..)
  , resolvePackage
  ) where

import Control.Lens
import Control.Monad.State
import Data.Char
import qualified Data.Foldable as Fold
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Traversable as T
import Data.Word
import System.FilePath
import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import Data.HPB.AST (Ident)
import qualified Data.HPB.AST as A
import Data.HPB.Partial

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail( MonadFail )
#endif

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

ppText :: Text -> Doc
ppText t = text (Text.unpack t)

isUpperRoman :: Char -> Bool
isUpperRoman c = 'A' <= c && c <= 'Z'

isLowerRoman :: Char -> Bool
isLowerRoman c = 'a' <= c && c <= 'z'

vcatPrefix1 :: Doc -> Doc -> [Doc] -> Doc
vcatPrefix1 _ _ [] = PP.empty
vcatPrefix1 ph pr (h:r) = hcat ((ph <> h <> line) : fmap (\v -> pr <> v <> line) r)

------------------------------------------------------------------------
-- ModuleName

data ModuleName = ModuleName [Text]

instance Pretty ModuleName where
  pretty (ModuleName l) = hcat (punctuate dot (ppText <$> l))

toUpperFirstName :: String -> Maybe Text
toUpperFirstName s = Text.pack <$> fixChars s
  where fixChars [] = Nothing
        fixChars (c:r) | isUpperRoman c = Just (c : map toIdentChar r)
                       | isLowerRoman c = Just (toUpper c : map toIdentChar r)
                       | otherwise = fixChars r

toLowerFirstName :: String -> Maybe String
toLowerFirstName s = fixChars s
  where fixChars [] = Nothing
        fixChars (c:r) | isUpperRoman c = Just (toLower c : map toIdentChar r)
                       | isLowerRoman c = Just (c : map toIdentChar r)
                       | otherwise = fixChars r

toIdentChar :: Char -> Char
toIdentChar c0 | isGood c0 = c0
               | otherwise = '_'
  where isGood c | isUpperRoman c = True
                 | isLowerRoman c = True
                 | isDigit c = True
                 | otherwise = False


moduleNameFromPackageName :: A.CompoundName -> Maybe ModuleName
moduleNameFromPackageName (A.CompoundName l) | null r = Nothing
                                             | otherwise = Just (ModuleName r)
  where r = mapMaybe f l
        f nm = toUpperFirstName (show (A.val nm))

moduleNameFromPath :: FilePath -> ModuleName
moduleNameFromPath path = ModuleName [fromMaybe "Main" (toUpperFirstName nm)]
  where nm = dropExtension $ takeFileName path

isIdentChar :: Char -> Bool
isIdentChar c = isLower c || isUpper c || isDigit c || c == '\''

isConId :: String -> Bool
isConId "" = False
isConId (c:r) = isUpper c && all isIdentChar r

moduleNameFromString :: MonadFail m => String -> m ModuleName
moduleNameFromString s0 = go [] s0
  where go r s = do
          case break (== '.') s of
            (c, _) | not (isConId c) -> fail $ "Invalid module name: " ++ s0
            (c, '.':next) -> go (fromString c:r) next
            (c, "") -> return $ ModuleName $ reverse $ fromString c:r
            (_, _) -> error $ "internal: moduleNameFromString " ++ s0

------------------------------------------------------------------------
-- Uncategorized

data FileDef
   = EnumDef EnumInfo
   | MessageDef MessageInfo

fileDefIdent :: FileDef -> Ident
fileDefIdent (EnumDef e) = enumIdent e
fileDefIdent (MessageDef m) = messageName m

------------------------------------------------------------------------
-- EnumInfo

data EnumInfo
   = EnumInfo { -- | Name used for haskell data type
                enumIdent :: !Ident
              , enumPos   :: !A.SourcePos
              , _enumValues     :: !(Seq Text)
              , _enumIdentMap   :: !(Map Ident A.SourcePos)
              , _enumCtors      :: !(Map Ident Word32)
              , _enumValueMap   :: !(Map Word32 [Ident])
              , _enumAllowAlias :: !(Maybe Bool)
              }

emptyEnumInfo :: A.Posd Ident -> EnumInfo
emptyEnumInfo (A.Posd nm p) =
  EnumInfo { enumIdent = nm
           , enumPos   = p
           , _enumValues     = Seq.empty
           , _enumIdentMap   = Map.empty
           , _enumCtors      = Map.empty
           , _enumValueMap   = Map.empty
           , _enumAllowAlias = Nothing
           }

-- | List of Haskell identifiers associated with each enum.
enumValues :: Lens' EnumInfo (Seq Text)
enumValues = lens _enumValues (\s v -> s { _enumValues = v })

-- | Map each identifier to it's known location.
enumIdentMap :: Lens' EnumInfo (Map Ident A.SourcePos)
enumIdentMap = lens _enumIdentMap (\s v -> s { _enumIdentMap = v })

-- | Map each identifier used as a constructor to it's value.
enumCtors :: Lens' EnumInfo (Map Ident Word32)
enumCtors = lens _enumCtors (\s v -> s { _enumCtors = v })

-- | Map each enum value to an associated identifier.
enumValueMap :: Lens' EnumInfo (Map Word32 [Ident])
enumValueMap = lens _enumValueMap (\s v -> s { _enumValueMap = v })

-- | Flag indicating if enum allows duplicates.
enumAllowAlias :: Lens' EnumInfo (Maybe Bool)
enumAllowAlias = lens _enumAllowAlias (\s v -> s { _enumAllowAlias = v })

enumHaskellType :: EnumInfo -> Doc
enumHaskellType = pretty . enumIdent

enumLegalValues :: EnumInfo -> Doc
enumLegalValues e = brackets (hcat (punctuate s (ppText <$> l)))
  where l = Fold.toList (e^.enumValues)
        s = text ", "

ppToEnumBindings :: EnumInfo -> Doc
ppToEnumBindings e = vcat (bindings ++ [end])
  where bindings = ppToEnumBinding <$> Map.toList (e^.enumValueMap)
        end = text "toEnum v = HPB.error (\""
              <> enumHaskellType e
              <+> text "given illegal value \" HPB.++ HPB.show v HPB.++ \".\")"

ppToEnumBinding :: (Word32, [Ident]) -> Doc
ppToEnumBinding (_,[]) = PP.empty
ppToEnumBinding (w,h:_) = text "toEnum" <+> int (fromIntegral w) <+> equals <+> pretty h

ppFromEnumBinding :: (Ident, Word32) -> Doc
ppFromEnumBinding (nm,w) = text "fromEnum" <+> pretty nm <+> equals <+> int (fromIntegral w)

ppEnumInfo :: EnumInfo -> Doc
ppEnumInfo e =
  text "data" <+> enumHaskellType e <$$>
  indent 3 (vcatPrefix1 (text "= ") (text "| ") (pretty <$> Map.keys (e^.enumCtors))) <>
  text "deriving (HPB.Eq, HPB.Ord, HPB.Show)" <$$>
  text "" <$$>
  text "instance HPB.Enum" <+> enumHaskellType e <+> text "where" <$$>
  indent 2 (ppToEnumBindings e) <$$>
  indent 2 (vcat (ppFromEnumBinding <$> Map.toList (e^.enumCtors))) <$$>
  text ""

type EnumWriter = StateT EnumInfo Partial

extractEnumFieldInfo :: A.EnumField -> EnumWriter ()
extractEnumFieldInfo (A.EnumOption (A.OptionDecl (A.Posd (A.KnownName "allow_alias") p) v)) = do
  old_val <- use enumAllowAlias
  when (isJust old_val) $ do
    A.failAt p $ "allow_alias already set."
  b <- A.asBoolVal "allow_alias expected Boolean value." v
  enumAllowAlias .= Just b
extractEnumFieldInfo (A.EnumOption _) = do
  return ()
extractEnumFieldInfo (A.EnumValue (A.Posd nm p) v) = do
  im <- use enumIdentMap
  case Map.lookup nm im of
    Just old_p -> A.failAt p $ show nm ++ " already declared at " ++ show old_p ++ "."
    Nothing -> return ()

  vm <- use enumValueMap
  let w = fromIntegral (A.numVal v)

  enumIdentMap %= Map.insert nm p
  case Map.lookup w vm of
    Nothing -> do
      enumCtors %= Map.insert nm w
      case toUpperFirstName (show nm) of
        Nothing -> A.failAt p $ show nm ++ " could not be converted to a Haskell constructor."
        Just t -> enumValues %= (Seq.|> t)
    Just{} -> do
      case toLowerFirstName (show nm) of
        Nothing -> A.failAt p $ show nm ++ " could not be converted to a lower case name."
        Just t -> enumValues %= (Seq.|> Text.pack t)
  enumValueMap %= Map.insertWith (++) w [nm]

------------------------------------------------------------------------
-- MessageInfo

-- | A message and list of fields prior to resolution.
data MessageInfo = MessageInfo { messageName :: Ident
                               , _messageFields :: Map Integer A.FieldDecl
                               }

-- | List of message fields ordered by tag.
messageFields :: Lens' MessageInfo (Map Integer A.FieldDecl)
messageFields = lens _messageFields (\s v -> s { _messageFields = v })

messageLowerPrefix :: MessageInfo -> String
messageLowerPrefix m =
  case toLowerFirstName (show (messageName m)) of
    Just s -> s
    Nothing -> error "Could not interpret message name as a Haskell identifier."

type MessageWriter = StateT MessageInfo Partial

extractMessageField :: A.MessageField -> MessageWriter ()
extractMessageField mf =
  case mf of
    A.MessageField f -> do
      m <- use messageFields
      let tag = A.numVal $ A.val $ A.fieldTag $ A.fieldDeclField f
      case Map.lookup tag m of
        Nothing -> return ()
        Just old_field -> do
          let nm     = show $ A.val $ A.fieldName $ A.fieldDeclField f
          let old_nm = show $ A.val $ A.fieldName $ A.fieldDeclField old_field
          let p = A.pos $ A.fieldName $ A.fieldDeclField f
          A.failAt p $ "The fields " ++ nm ++ " and " ++ old_nm ++ " assigned the same tag."
      messageFields .= Map.insert tag f m
    A.MessageOption _ -> do
      return ()
    A.OneOf nm _ -> do
      A.failAt (A.pos nm) $ "oneof is not yet supported."
    A.Extensions p _ _ -> do
      A.failAt p $ "Extensions are not yet supported."
    A.LocalEnum e -> do
      A.failAt (A.enumPos e) $ "Local enumerations are not yet supported."
    A.LocalMessage m -> do
      A.failAt (A.pos (A.messageName m)) $ "Local messages are not yet supported."
    A.LocalExtend e -> do
      A.failAt (A.pos (A.extendMessage e)) $ "Extensions are not yet supported."

------------------------------------------------------------------------
-- FileContext

data FileContext = FCtx { _fcIdentMap   :: !(Map Ident A.SourcePos)
                        , _fcDefs       :: !(Seq FileDef)
                        }

fcIdentMap :: Lens' FileContext (Map Ident A.SourcePos)
fcIdentMap = lens _fcIdentMap (\s v -> s { _fcIdentMap = v })

fcDefs :: Lens' FileContext (Seq FileDef)
fcDefs = lens _fcDefs (\s v -> s { _fcDefs = v })

emptyFileContext :: FileContext
emptyFileContext =
  FCtx { _fcIdentMap = Map.empty
       , _fcDefs     = Seq.empty
       }

type FileWriter = StateT FileContext Partial

reserveName :: A.SourcePos -> Ident -> FileWriter ()
reserveName p nm = do
  m <- use fcIdentMap
  case Map.lookup nm m of
    Nothing -> fcIdentMap %= Map.insert nm p
    Just old_p -> A.failAt p $ show nm ++ " already defined at " ++ show old_p ++ "."

extractFileEnum :: A.EnumDecl -> FileWriter ()
extractFileEnum d = do
  let A.Posd nm p = A.enumIdent d
  reserveName p nm

  e <- lift $ flip execStateT (emptyEnumInfo (A.enumIdent d)) $ do
         mapM_ extractEnumFieldInfo (A.enumFields d)
  when (Map.null (e^.enumCtors)) $ do
    A.failAt p $ show nm ++ " must contain at least one value."
  when (e^.enumAllowAlias /= Just True) $ do
    let isDup (_nm, l) = length l > 1
    case filter isDup $ Map.toList (e^.enumValueMap) of
      [] -> return ()
      _ -> fail $ show nm ++ " contains aliases, but allow_alias is not set."
  fcDefs %= (Seq.|> EnumDef e)

extractMessage :: A.MessageDecl -> FileWriter ()
extractMessage (A.MessageDecl (A.Posd nm p) fields) = do
  reserveName p nm
  let m0 = MessageInfo { messageName = nm
                       , _messageFields = Map.empty
                       }
  m <- lift $ flip execStateT m0 $ do
         mapM_ extractMessageField fields
  fcDefs %= (Seq.|> MessageDef m)

extractFileDecl :: A.Decl -> FileWriter ()
extractFileDecl decl = do
  case decl of
    A.Import _ _ -> do
      fail "hdb does not yet support imports."
    A.Option (A.OptionDecl (A.Posd _ _) _) ->
      return ()
    A.Enum d     -> extractFileEnum d
    A.Message d  -> extractMessage d
    A.Extend _   -> fail "hdb does not yet support message extensions."
    A.Service _  -> fail "hdb does not yet support service declarations."

------------------------------------------------------------------------
-- TypeContext

data TypeContext = TypeContext { globalIdentMap :: !(Map Ident FileDef)
                               , _localIdentMap :: !(Map Ident FileDef)
                               , _localEnumMap :: !(Map Ident Text)
                               }

localIdentMap :: Lens' TypeContext (Map Ident FileDef)
localIdentMap = lens _localIdentMap (\s v -> s { _localIdentMap = v })

localEnumMap :: Lens' TypeContext (Map Ident Text)
localEnumMap = lens _localEnumMap (\s v -> s { _localEnumMap = v })

lookupGlobalType :: MonadFail m
                 => TypeContext -> A.CompoundName -> m FileDef
lookupGlobalType ctx = resolveIdentType (globalIdentMap ctx)

lookupLocalType :: MonadFail m
                => TypeContext -> A.CompoundName -> m FileDef
lookupLocalType ctx = resolveIdentType (ctx^.localIdentMap)

resolveIdentType :: MonadFail m
                 => Map Ident FileDef
                 -> A.CompoundName
                 -> m FileDef
resolveIdentType m0 cnm@(A.CompoundName l0) = go m0 l0
  where go _ [] = error $ "internal: Could not resolve " ++ show (pretty cnm) ++ "."
        go m (nm:l) = do
          d <- go1 m nm
          case (d,l) of
            (_,[]) -> return d
            (EnumDef{}, _) -> A.failAt (A.pos nm) $ "Could not resolve " ++ show (pretty cnm) ++ "."
            (MessageDef{}, _) -> do
              --TODO: Add support for compound names.
              A.failAt (A.pos nm) $ "Could not resolve " ++ show (pretty cnm) ++ "."
        go1 m (A.Posd nm p) = do
          case Map.lookup nm m of
            Nothing -> A.failAt p $ "Could not resolve " ++ show (pretty cnm) ++ "."
            Just d -> return d

lookupEnumCtor :: MonadFail m => TypeContext -> A.SourcePos -> Ident -> m Text
lookupEnumCtor ctx p i = do
  case Map.lookup i (ctx^.localEnumMap) of
    Nothing -> A.failAt p $ "Unknown identifier: " ++ show i ++ "."
    Just v -> return v

mkTypeContext :: FileContext -> TypeContext
mkTypeContext ctx =
    TypeContext { globalIdentMap = typeMap
                , _localIdentMap = typeMap
                , _localEnumMap  = enumMap
                }
  where defs = Fold.toList (ctx^.fcDefs)
        typeMap = Map.fromList [ (fileDefIdent d, d) | d <- defs ]
        enums = [ e | EnumDef e <- defs ]
        enumMap = Map.fromList [ (A.Ident t, t)
                               | e <- enums
                               , t <- Fold.toList (e^.enumValues)
                               ]

------------------------------------------------------------------------
-- ResolvedVal

data ResolvedVal
   = NumVal A.NumLit
   | EnumCtor Text
     -- ^ An enumeration constructor with the given identifier.
   | StringVal A.StringLit
   | BoolVal Bool

ppResolvedVal :: Int -> ResolvedVal -> Doc
ppResolvedVal prec rv =
  case rv of
    NumVal x -> pretty x
    EnumCtor x -> ppText x
    StringVal (A.StringLit s) ->
      parensIf (prec >= 10) $ text "HPB.fromString" <+> text (show s)
    BoolVal True  ->  text "HPB.True"
    BoolVal False ->  text "HPB.False"

resolveValue :: (Functor m, MonadFail m) => TypeContext -> A.Posd A.Val -> m ResolvedVal
resolveValue ctx (A.Posd v p) =
  case v of
    A.NumVal    x -> return $ NumVal x
    A.IdentVal  x -> EnumCtor <$> lookupEnumCtor ctx p x
    A.StringVal s -> return $ StringVal s
    A.BoolVal   b -> return $ BoolVal b

------------------------------------------------------------------------
-- ResolvedType

data ResolvedType
   = ScalarType A.ScalarType
     -- | A message with the given name.
   | MessageType Text
     -- | An enumeration with the given information.
   | EnumType EnumInfo

ppFieldType :: ResolvedType -> Doc
ppFieldType (ScalarType tp) = text $ ("HPB." ++) $
  case tp of
    A.DoubleType   -> "Double"
    A.FloatType    -> "Float"
    A.Int32Type    -> "Int32"
    A.Int64Type    -> "Int64"
    A.Uint32Type   -> "Word32"
    A.Uint64Type   -> "Word64"
    A.Sint32Type   -> "Int32"
    A.Sint64Type   -> "Int64"
    A.Fixed32Type  -> "Word32"
    A.Fixed64Type  -> "Word64"
    A.Sfixed32Type -> "Int32"
    A.Sfixed64Type -> "Int64"
    A.BoolType     -> "Bool"
    A.StringType   -> "Text"
    A.BytesType    -> "ByteString"
ppFieldType (MessageType nm) = ppText nm
ppFieldType (EnumType e) = pretty (enumIdent e)

ppResolvedTypeDefault :: Int -> ResolvedType -> Doc
ppResolvedTypeDefault prec rtp =
  case rtp of
    ScalarType tp -> ppScalarDefault prec tp
    MessageType _ -> text "HPB.mempty"
    EnumType e -> case Seq.viewl (e^.enumValues) of
                    Seq.EmptyL -> error "illegal: Enumerator constains no elements."
                    h Seq.:< _ -> ppText h

resolveFileDefType :: (Functor m, MonadFail m) => FileDef -> m ResolvedType
resolveFileDefType (EnumDef e) = return (EnumType e)
resolveFileDefType (MessageDef m) = do
  let nm = show (messageName m)
  case toUpperFirstName nm of
    Just t -> return (MessageType t)
    Nothing -> fail $ "Could not resolve message " ++ nm ++ " as a Haskell constructor."

resolveFieldType :: (Functor m, MonadFail m)
                 => TypeContext
                 -> A.Posd A.FieldType
                 -> m ResolvedType
resolveFieldType _ (A.Posd (A.ScalarFieldType tp) _) =
  return $ ScalarType tp
resolveFieldType ctx (A.Posd (A.NamedFieldType tp) _p) =
  resolveFileDefType =<< lookupLocalType ctx tp
resolveFieldType ctx (A.Posd (A.GlobalNamedType tp) _p) =
  resolveFileDefType =<< lookupGlobalType ctx tp

------------------------------------------------------------------------
-- ResolvedFieldInfo

-- | A field in a message with all information needed to pretty-print the
-- message to Haskell.
data ResolvedFieldInfo = RFI { rfiIdent :: !Ident
                               -- | Name of lens for field as a string.
                             , rfiLensName :: !String
                             , rfiRule :: !A.FieldRule
                             , rfiType :: !ResolvedType
                               -- | The unique numbered tag associated with the field.
                             , rfiTag  :: !A.NumLit
                             , rfiDefault :: !(Maybe ResolvedVal)
                             , rfiIsPacked :: !Bool
                             }

-- | Get name of lens associated with field.
fieldLensName :: String -> A.FieldRule -> Ident -> String
fieldLensName prefix rl nm =
    case toLowerFirstName snm of
      Nothing -> error "Could not interpret field name as lens."
      Just s -> prefix ++ "_" ++ s
  where snm = case rl of
                A.Repeated -> show nm ++ "s"
                _ -> show nm

resolveFieldInfo :: (Functor m, MonadFail m)
                 => TypeContext
                 -> String
                    -- ^ Prefix if field identifier has already been used.
                 -> A.FieldDecl
                 -> m ResolvedFieldInfo
resolveFieldInfo ctx prefix (A.FieldDecl rl f) = do
  tp <- resolveFieldType ctx (A.fieldType f)
  rv <- T.mapM (resolveValue ctx) (A.fieldDefault f)
  let nm = A.val (A.fieldName f)
  packed <- A.fieldIsPacked f
  return RFI { rfiIdent = nm
             , rfiLensName = fieldLensName prefix rl nm
             , rfiRule = rl
             , rfiType = tp
             , rfiTag  = A.val (A.fieldTag f)
             , rfiDefault = rv
             , rfiIsPacked = packed
             }

rfiRecordFieldName :: ResolvedFieldInfo -> Doc
rfiRecordFieldName f = text ('_' : rfiLensName f)

ppRfiType :: Int -> ResolvedFieldInfo -> Doc
ppRfiType prec f =
  case rfiRule f of
    A.Repeated -> parensIf (prec >= 10) (text "HPB.Seq" <+> ppFieldType (rfiType f))
    _ -> ppFieldType (rfiType f)

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

ppFieldDecl :: ResolvedFieldInfo -> Doc
ppFieldDecl f = rfiRecordFieldName f <+> text "::" <+> ppRfiType 0 f

-- | Print the default value to initialzie a field with.
ppFieldDefault :: Int -> ResolvedFieldInfo -> Doc
ppFieldDefault prec f =
  case rfiRule f of
    A.Repeated -> text "HPB.mempty"
    _ | Just v <- rfiDefault f -> ppResolvedVal prec v
      | otherwise -> ppResolvedTypeDefault prec (rfiType f)

ppFieldInit :: ResolvedFieldInfo -> Doc
ppFieldInit rfi =
   rfiRecordFieldName rfi <+> text "=" <+> ppFieldDefault 0 rfi

ppFieldLens :: Doc -> ResolvedFieldInfo -> Doc
ppFieldLens messageType f =
    l_nm <+> text ":: HPB.Lens'" <+> messageType <+> ppRfiType 10 f <$$>
    l_nm <+> text "= HPB.lens" <+> rec_nm <+> setter <$$>
    line
 where l_nm = text (rfiLensName f)
       rec_nm = rfiRecordFieldName f
       setter = text "(\\s v -> s {" <+> rec_nm <+> text "= v })"

ppFieldAppend :: String -> String -> ResolvedFieldInfo -> Doc
ppFieldAppend x y f = rfiRecordFieldName f <+> text "=" <+> rhs
  where rhs | shouldMergeFields f =
              rfiRecordFieldName f <+> text x
                <+> text "HPB.<>"
                <+> rfiRecordFieldName f <+> text y
            | otherwise = rfiRecordFieldName f <+> text y

ppFieldDef :: ResolvedFieldInfo -> Doc
ppFieldDef f = do
  let f_nm = parens (text "HPB.fromString" <+> text (show (show (rfiIdent f))))
  let packed = text (if rfiIsPacked f then "HPB.Packed" else "HPB.Unpacked")
  let tag = text (show (rfiTag f))
  let req = case rfiRule f of
              A.Required -> text "HPB.Req"
              A.Optional -> parens (text "HPB.Opt" <+> ppFieldDefault 10 f)
              _ -> error "internal: Default value not used in repeated fields."
  let lens_nm = text (rfiLensName f)
  case rfiType f of
    ScalarType tp -> do
      let prefix =
            case tp of
              A.DoubleType   -> "double"
              A.FloatType    -> "float"
              A.Int32Type    -> "int32"
              A.Int64Type    -> "int64"
              A.Uint32Type   -> "uint32"
              A.Uint64Type   -> "uint64"
              A.Sint32Type   -> "sint32"
              A.Sint64Type   -> "sint64"
              A.Fixed32Type  -> "fixed32"
              A.Fixed64Type  -> "fixed64"
              A.Sfixed32Type -> "sfixed32"
              A.Sfixed64Type -> "sfixed64"
              A.BoolType     -> "bool"
              A.StringType   -> "string"
              A.BytesType    -> "bytes"
      let showPacked
            | (tp == A.StringType) || (tp == A.BytesType) = text ""
            | otherwise = text " " <> packed
      case rfiRule f of
        A.Repeated ->
           text ("HPB." ++ prefix ++ "RepeatedField") <+> f_nm <> showPacked <+> tag <+> lens_nm
        _ -> text ("HPB." ++ prefix ++ "Field") <+> f_nm <+> req <+> tag <+> lens_nm
    MessageType _ -> do
      case rfiRule f of
        A.Required ->
          text "HPB.messageField HPB.messageRep" <+> f_nm <+> text "HPB.True"  <+> tag <+> lens_nm
        A.Optional ->
          text "HPB.messageField HPB.messageRep" <+> f_nm <+> text "HPB.False" <+> tag <+> lens_nm
        A.Repeated ->
          text "HPB.messageRepeatedField HPB.messageRep" <+> f_nm <+> tag <+> lens_nm
    EnumType e -> do
      let vals = enumLegalValues e
      case rfiRule f of
        A.Repeated ->
          text "HPB.enumRepeatedField" <+> f_nm <+> packed <+> vals <+> tag <+> lens_nm
        _ -> text "HPB.enumField" <+> f_nm <+> req <+> vals <+> tag <+> lens_nm

shouldMergeFields :: ResolvedFieldInfo -> Bool
shouldMergeFields f
  | A.Repeated <- rfiRule f = True
  | MessageType _ <- rfiType f = True
  | otherwise = False

------------------------------------------------------------------------
-- ResolvedMessage

-- | A protocol buffers message that has been resolved so that it may
-- be pretty-printed to Haskell.
data ResolvedMessage
   = RM { rmName :: Ident
        , rmFields :: [ResolvedFieldInfo]
        }

resolveMessage :: (Functor m, MonadFail m) => TypeContext -> MessageInfo -> m ResolvedMessage
resolveMessage ctx m = do
  let msg_nm = messageLowerPrefix m
  fields <- mapM (resolveFieldInfo ctx msg_nm) (Map.elems (m^.messageFields))
  return RM { rmName = messageName m
            , rmFields = fields
            }

ppResolvedMessage :: ResolvedMessage -> Doc
ppResolvedMessage m =
    text "data" <+> pretty nm <$$>
    text "   =" <+> pretty nm <+> text "{" <$$>
    indent 3 (vcatPrefix1 (text "  ") (text ", ") (ppFieldDecl <$> fields)) <>
    text "}" <$$>
    text "" <$$>

    text "instance HPB.Semigroup" <+> pretty nm <+> text "where" <$$>
    text "  (<>) x y =" <+> pretty nm <+> text "{" <$$>
    indent 17 (vcatPrefix1 (text "  ") (text ", ") (ppFieldAppend "x" "y" <$> fields)) <>
    text "}" <$$>
    text "" <$$>

    text "instance HPB.Monoid" <+> pretty nm <+> text "where" <$$>
    text "  mempty =" <+> pretty nm <+> text "{" <$$>
    indent 12 (vcatPrefix1 (text "  ") (text ", ") (ppFieldInit <$> fields)) <>
    text "}" <$$>
    text "  mappend x y =" <+> pretty nm <+> text "{" <$$>
    indent 17 (vcatPrefix1 (text "  ") (text ", ") (ppFieldAppend "x" "y" <$> fields)) <>
    text "}" <$$>
    text "" <$$>

    hcat (ppFieldLens (pretty nm) <$> fields) <>
    text "instance HPB.HasMessageRep" <+> pretty nm <+> text "where" <$$>
    text "  messageRep" <$$>
    text "    = HPB.emptyMessageRep (HPB.fromString" <+> nm_as_string <> text ")" <$$>
    indent 4 (vcat ((\f -> text "HPB.&" <+> ppFieldDef f) <$> fields)) <$$>
    text ""
  where nm = rmName m
        fields = rmFields m
        nm_as_string = text (show (show nm))

ppScalarDefault :: Int -> A.ScalarType -> Doc
ppScalarDefault prec tp =
  case tp of
    A.DoubleType   -> text "0"
    A.FloatType    -> text "0"
    A.Int32Type    -> text "0"
    A.Int64Type    -> text "0"
    A.Uint32Type   -> text "0"
    A.Uint64Type   -> text "0"
    A.Sint32Type   -> text "0"
    A.Sint64Type   -> text "0"
    A.Fixed32Type  -> text "0"
    A.Fixed64Type  -> text "0"
    A.Sfixed32Type -> text "0"
    A.Sfixed64Type -> text "0"
    A.BoolType     -> text "HPB.False"
    A.StringType   -> parensIf (prec >= 10) (text "HPB.fromString \"\"")
    A.BytesType    -> text "HPB.mempty"

------------------------------------------------------------------------
-- ResolvedDef

data ResolvedDef
   = ResolvedEnum EnumInfo
   | ResolvedMessage ResolvedMessage

instance Pretty ResolvedDef where
  pretty (ResolvedEnum e) = ppEnumInfo e
  pretty (ResolvedMessage m) = ppResolvedMessage m

resolveFileDef :: (Functor m, MonadFail m) => TypeContext -> FileDef -> m ResolvedDef
resolveFileDef _ (EnumDef e) = return (ResolvedEnum e)
resolveFileDef ctx (MessageDef m) =
  ResolvedMessage <$> resolveMessage ctx m

------------------------------------------------------------------------
-- Package

data Package = Package { haskellModuleName :: !ModuleName
                       , moduleDefs :: ![ResolvedDef]
                       }

instance Pretty Package where
  pretty pkg =
    text "{-# OPTIONS_GHC -fno-warn-unused-matches #-}" <$$>
    text "module" <+> pretty (haskellModuleName pkg) <+> text "where" <$$>
    text "import qualified Data.HPB as HPB" <$$>
    text "import Prelude ()" <$$>
    text "" <$$>
    vcat (pretty <$> moduleDefs pkg)

resolvePackage :: FilePath -> Maybe String -> A.Package -> Either String Package
resolvePackage path mnm (A.Package pkg_nm decls) = runPartial $ do
  ctx <-
    flip execStateT emptyFileContext $ do
      mapM_ extractFileDecl decls
  let default_nm = fromMaybe (moduleNameFromPath path)
                             (moduleNameFromPackageName =<< pkg_nm)
  nm <- case mnm of
          Just nm -> moduleNameFromString nm
          Nothing -> return default_nm
  let tpCtx = mkTypeContext ctx
  defs <- mapM (resolveFileDef tpCtx) (Fold.toList (ctx^.fcDefs))
  return Package { haskellModuleName = nm
                 , moduleDefs = defs
                 }
