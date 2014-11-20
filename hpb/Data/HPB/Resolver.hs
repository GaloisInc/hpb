{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Data.HPB.Resolver
  ( Package
  , Message
  , A.CompoundName
  , resolvePackage
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Error
import Control.Monad.State
import Data.Char
import qualified Data.Foldable as Fold
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Traversable as T
import Data.Word
import System.FilePath
import Text.PrettyPrint.Leijen as PP hiding ((<$>))

import Data.HPB.AST (Ident)
import qualified Data.HPB.AST as A
import qualified Data.HPB.Parser as P

ppText :: Text -> Doc
ppText t = text (Text.unpack t)

isUpperRoman :: Char -> Bool
isUpperRoman c = 'A' <= c && c <= 'Z'

isLowerRoman :: Char -> Bool
isLowerRoman c = 'a' <= c && c <= 'z'

failAt :: Monad m => A.SourcePos -> String -> m a
failAt p msg = fail $ show $
  pretty p <> text ":" <$$>
  indent 2 (text msg)

------------------------------------------------------------------------
-- Value utilities

asBoolVal :: Monad m => String -> A.Posd A.Val -> m Bool
asBoolVal msg (A.Posd v p) =
  case v of
    A.BoolVal b -> return b
    _ -> failAt p msg

asStringVal :: Monad m => String -> A.Posd A.Val -> m Text
asStringVal msg (A.Posd v p) =
  case v of
    A.StringVal s -> return (A.stringText s)
    _ -> failAt p msg

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

moduleNameFromString :: Monad m => A.SourcePos -> String -> m ModuleName
moduleNameFromString p s0 = go [] s0
  where go r s = do
          case break (== '.') s of
            (c, _) | not (isConId c) -> failAt p $ "Invalid module name: " ++ s0
            (c, '.':next) -> go (fromString c:r) next
            (c, "") -> return $ ModuleName $ reverse $ fromString c:r
            (c, _) -> error $ "internal: moduleNameFromString " ++ s0

------------------------------------------------------------------------
-- Uncategorized

type PackageName = A.CompoundName


type HaskellModuleName = Text

data FileDef
   = EnumDef EnumInfo
   | MessageDef MessageInfo

fileDefIdent :: FileDef -> Ident
fileDefIdent (EnumDef e) = enumIdent e
fileDefIdent (MessageDef m) = messageName m

type FullyQualifiedMessageName = A.CompoundName

data Message = Message { messageCtor :: !Text
                       }

ppCompoundName :: A.CompoundName -> Doc
ppCompoundName (A.CompoundName l) =
  case toUpperFirstName (Text.unpack (Text.intercalate "_" (A.identText . A.val <$> l))) of
    Nothing -> error "Cannot print compoundName"
    Just t -> ppText t

vcatPrefix1 :: Doc -> Doc -> [Doc] -> Doc
vcatPrefix1 _ _ [] = PP.empty
vcatPrefix1 ph pr (h:r) = hcat ((ph <> h <> line) : fmap (\v -> pr <> v <> line) r)

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
enumValues :: Simple Lens EnumInfo (Seq Text)
enumValues = lens _enumValues (\s v -> s { _enumValues = v })

-- | Map each identifier to it's known location.
enumIdentMap :: Simple Lens EnumInfo (Map Ident A.SourcePos)
enumIdentMap = lens _enumIdentMap (\s v -> s { _enumIdentMap = v })

-- | Map each identifier used as a constructor to it's value.
enumCtors :: Simple Lens EnumInfo (Map Ident Word32)
enumCtors = lens _enumCtors (\s v -> s { _enumCtors = v })

-- | Map each enum value to an associated identifier.
enumValueMap :: Simple Lens EnumInfo (Map Word32 [Ident])
enumValueMap = lens _enumValueMap (\s v -> s { _enumValueMap = v })

-- | Flag indicating if enum allows duplicates.
enumAllowAlias :: Simple Lens EnumInfo (Maybe Bool)
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
        end = text "toEnum v = error (\""
              <> enumHaskellType e
              <+> text "given illegal value \" ++ show v ++ \".\")"

ppToEnumBinding :: (Word32, [Ident]) -> Doc
ppToEnumBinding (_,[]) = PP.empty
ppToEnumBinding (w,h:_) = text "toEnum" <+> int (fromIntegral w) <+> equals <+> pretty h

ppFromEnumBinding :: (Ident, Word32) -> Doc
ppFromEnumBinding (nm,w) = text "fromEnum" <+> pretty nm <+> equals <+> int (fromIntegral w)

ppEnumInfo :: EnumInfo -> Doc
ppEnumInfo e =
  text "data" <+> enumHaskellType e <$$>
  indent 3 (vcatPrefix1 (text "= ") (text "| ") (pretty <$> Map.keys (e^.enumCtors))) <>
  text "deriving (Eq, Ord)" <$$>
  text "" <$$>
  text "instance Enum" <+> enumHaskellType e <+> text "where" <$$>
  indent 2 (ppToEnumBindings e) <$$>
  indent 2 (vcat (ppFromEnumBinding <$> Map.toList (e^.enumCtors))) <$$>
  text ""

type EnumWriter = StateT EnumInfo (ErrorT String Identity)

extractEnumFieldInfo :: A.EnumField -> EnumWriter ()
extractEnumFieldInfo (A.EnumOption (A.OptionDecl (A.Posd (A.KnownName "allow_alias") p) v)) = do
  old_val <- use enumAllowAlias
  when (isJust old_val) $ do
    failAt p $ "allow_alias already set."
  b <- asBoolVal "allow_alias expected Boolean value." v
  enumAllowAlias .= Just b
extractEnumFieldInfo (A.EnumOption _) = do
  return ()
extractEnumFieldInfo (A.EnumValue (A.Posd nm p) v) = do
  im <- use enumIdentMap
  case Map.lookup nm im of
    Just old_p -> failAt p $ show nm ++ " already declared at " ++ show old_p ++ "."
    Nothing -> return ()

  vm <- use enumValueMap
  let w = fromIntegral (A.numVal v)

  enumIdentMap %= Map.insert nm p
  case Map.lookup w vm of
    Nothing -> do
      enumCtors %= Map.insert nm w
      case toUpperFirstName (show nm) of
        Nothing -> failAt p $ show nm ++ " could not be converted to a Haskell constructor."
        Just t -> enumValues %= (Seq.|> t)
    Just{} -> do
      case toLowerFirstName (show nm) of
        Nothing -> failAt p $ show nm ++ " could not be converted to a lower case name."
        Just t -> enumValues %= (Seq.|> Text.pack t)
  enumValueMap %= Map.insertWith (++) w [nm]


------------------------------------------------------------------------
-- MessageInfo

data FieldInfo = FieldInfo { fieldPos :: !A.SourcePos
                           , fieldName :: !Ident
                           , fieldTag  :: !A.NumLit
                           , fieldRule :: !A.FieldRule
                           , fieldType :: !(A.Posd A.FieldType)
                           , _fieldDefault :: !(Maybe (A.Posd A.Val))
                           , _fieldIsPacked :: !Bool
                           }

fieldDefault :: Simple Lens FieldInfo (Maybe (A.Posd A.Val))
fieldDefault = lens _fieldDefault (\s v -> s { _fieldDefault = v })

fieldIsPacked :: Simple Lens FieldInfo Bool
fieldIsPacked = lens _fieldIsPacked (\s v -> s { _fieldIsPacked = v })

type MessageFieldWriter = StateT FieldInfo (ErrorT String Identity)


extractMessageFieldOptions :: A.OptionDecl -> MessageFieldWriter ()
extractMessageFieldOptions (A.OptionDecl (A.Posd nm p) v) = do
  case nm of
    A.KnownName "default" -> fieldDefault .= Just v
    A.KnownName "packed" -> do
      b <- asBoolVal "packed value must be a Boolean." v
      fieldIsPacked .= b
    _ -> return ()

data MessageInfo = MessageInfo { messageName :: Ident
                               , _messageFields :: Seq FieldInfo
                               }

messageFields :: Simple Lens MessageInfo (Seq FieldInfo)
messageFields = lens _messageFields (\s v -> s { _messageFields = v })

type MessageWriter = StateT MessageInfo (ErrorT String Identity)

extractMessageField :: A.MessageField -> MessageWriter ()
extractMessageField mf =
  case mf of
    A.MessageField (A.FieldDecl rl f) -> do
      let opts = A.fieldOptions f
      let f0 = FieldInfo { fieldPos = A.pos (A.fieldName f)
                         , fieldName = A.val (A.fieldName f)
                         , fieldTag = A.val (A.fieldTag f)
                         , fieldRule = rl
                         , fieldType = A.fieldType f
                         , _fieldDefault = Nothing
                         , _fieldIsPacked = False
                         }
      f <- lift $ flip execStateT f0 $ do
             mapM_ extractMessageFieldOptions (A.fieldOptions f)
      messageFields %= (Seq.|> f)
    A.MessageOption _ -> do
      return ()
    A.OneOf nm _ -> do
      failAt (A.pos nm) $ "oneof is not yet supported."
    A.Extensions p _ _ -> do
      failAt p $ "Extensions are not yet supported."
    A.LocalEnum e -> do
      failAt (A.enumPos e) $ "Local enumerations are not yet supported."
    A.LocalMessage m -> do
      failAt (A.pos (A.messageName m)) $ "Local messages are not yet supported."
    A.LocalExtend e -> do
      failAt (A.pos (A.extendMessage e)) $ "Extensions are not yet supported."

------------------------------------------------------------------------
-- FileContext

data FileContext = FCtx { _fcModuleName :: !(Maybe ModuleName)
                        , _fcIdentMap   :: !(Map Ident A.SourcePos)
                        , _fcDefs       :: !(Seq FileDef)
                        }

-- | Return module name associated with file.
fcModuleName :: Simple Lens FileContext (Maybe ModuleName)
fcModuleName = lens _fcModuleName (\s v -> s { _fcModuleName = v })

fcIdentMap :: Simple Lens FileContext (Map Ident A.SourcePos)
fcIdentMap = lens _fcIdentMap (\s v -> s { _fcIdentMap = v })

fcDefs :: Simple Lens FileContext (Seq FileDef)
fcDefs = lens _fcDefs (\s v -> s { _fcDefs = v })

emptyFileContext :: FileContext
emptyFileContext  =
  FCtx { _fcModuleName = Nothing
       , _fcIdentMap = Map.empty
       , _fcDefs     = Seq.empty
       }

type FileWriter = StateT FileContext (ErrorT String Identity)

extractFileOption :: A.SourcePos -> A.OptionName -> A.Posd A.Val -> FileWriter ()
extractFileOption p (A.KnownName "haskell_module") v = do
  txt <- asStringVal "haskell_module name must be a string." v
  nm <- moduleNameFromString p (Text.unpack txt)
  fcModuleName .= Just nm
extractFileOption _ _ _ =
  return ()

reserveName :: A.SourcePos -> Ident -> FileWriter ()
reserveName p nm = do
  m <- use fcIdentMap
  case Map.lookup nm m of
    Nothing -> fcIdentMap %= Map.insert nm p
    Just old_p -> failAt p $ show nm ++ " already defined at " ++ show old_p ++ "."

extractFileEnum :: A.EnumDecl -> FileWriter ()
extractFileEnum d = do
  let A.Posd nm p = A.enumIdent d
  reserveName p nm

  e <- lift $ flip execStateT (emptyEnumInfo (A.enumIdent d)) $ do
         mapM_ extractEnumFieldInfo (A.enumFields d)
  when (Map.null (e^.enumCtors)) $ do
    failAt p $ show nm ++ " must contain at least one value."
  when (e^.enumAllowAlias /= Just True) $ do
    let isDup (nm, l) = length l > 1
    case filter isDup $ Map.toList (e^.enumValueMap) of
      [] -> return ()
      _ -> fail $ show nm ++ " contains aliases, but allow_alias is not set."
  fcDefs %= (Seq.|> EnumDef e)

extractMessage :: A.MessageDecl -> FileWriter ()
extractMessage (A.MessageDecl (A.Posd nm p) fields) = do
  reserveName p nm
  let m0 = MessageInfo { messageName = nm
                       , _messageFields = Seq.empty
                       }
  m <- lift $ flip execStateT m0 $ do
         mapM_ extractMessageField fields
  fcDefs %= (Seq.|> MessageDef m)

extractFileDecl :: A.Decl -> FileWriter ()
extractFileDecl decl = do
  case decl of
    A.Import _ _ -> do
      fail "hdb does not yet support imports."
    A.Option (A.OptionDecl (A.Posd nm p) v) ->
      extractFileOption p nm v
    A.Enum d     -> extractFileEnum d
    A.Message d  -> extractMessage d
    A.Extend _   -> fail "hdb does not yet support message extensions."
    A.Service _  -> fail "hdb does not yet support service declarations."

mkFileContext :: A.Package -> ErrorT String Identity FileContext
mkFileContext (A.Package pkg_nm decls) = execStateT go emptyFileContext
  where go = do
          mapM_ extractFileDecl decls
          -- Assign module name if not defined.
          mdef <- use fcModuleName
          case mdef of
            Just{} -> return ()
            Nothing -> fcModuleName .= (moduleNameFromPackageName =<< pkg_nm)

------------------------------------------------------------------------
-- TypeContext

data TypeContext = TypeContext { globalIdentMap :: !(Map Ident FileDef)
                               , _localIdentMap :: !(Map Ident FileDef)
                               , _localEnumMap :: !(Map Ident Text)
                               }

localIdentMap :: Simple Lens TypeContext (Map Ident FileDef)
localIdentMap = lens _localIdentMap (\s v -> s { _localIdentMap = v })

localEnumMap :: Simple Lens TypeContext (Map Ident Text)
localEnumMap = lens _localEnumMap (\s v -> s { _localEnumMap = v })

lookupGlobalType :: Monad m
                 => TypeContext -> A.CompoundName -> m FileDef
lookupGlobalType ctx = resolveIdentType (globalIdentMap ctx)

lookupLocalType :: Monad m
                => TypeContext -> A.CompoundName -> m FileDef
lookupLocalType ctx = resolveIdentType (ctx^.localIdentMap)

resolveIdentType :: Monad m
                 => Map Ident FileDef
                 -> A.CompoundName
                 -> m FileDef
resolveIdentType m0 cnm@(A.CompoundName l) = go m0 l
  where go _ [] = error $ "internal: Could not resolve " ++ show (pretty cnm) ++ "."
        go m (nm:l) = do
          d <- go1 m nm
          case (d,l) of
            (_,[]) -> return d
            (EnumDef{}, _) -> failAt (A.pos nm) $ "Could not resolve " ++ show (pretty cnm) ++ "."
            (MessageDef{}, _) -> do
              --TODO: Add support for compound names.
              failAt (A.pos nm) $ "Could not resolve " ++ show (pretty cnm) ++ "."
        go1 m (A.Posd nm p) = do
          case Map.lookup nm m of
            Nothing -> failAt p $ "Could not resolve " ++ show (pretty cnm) ++ "."
            Just d -> return d

lookupEnumCtor :: Monad m => TypeContext -> A.SourcePos -> Ident -> m Text
lookupEnumCtor ctx p i = do
  case Map.lookup i (ctx^.localEnumMap) of
    Nothing -> failAt p $ "Unknown identifier: " ++ show i ++ "."
    Just v -> return v

mkTypeContext :: FileContext -> TypeContext
mkTypeContext ctx =
    TypeContext { globalIdentMap = typeMap
                , _localIdentMap = typeMap
                , _localEnumMap  = enumMap
                }
  where defs = Fold.toList (ctx^.fcDefs)
        typeMap = Map.fromList [ (fileDefIdent d, d) | d <- defs ]
        enumMap = Map.fromList [ (A.Ident t, t)
                               | EnumDef e <- defs
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

instance Pretty ResolvedVal where
  pretty (NumVal x) = pretty x
  pretty (EnumCtor x) = ppText x
  pretty (StringVal (A.StringLit s)) = text "fromString" <+> text (show s)
  pretty (BoolVal b) = text (show b)

resolveValue :: (Functor m, Monad m) => TypeContext -> A.Posd A.Val -> m ResolvedVal
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
ppFieldType (ScalarType tp) = text $
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

ppResolvedTypeDefault :: ResolvedType -> Doc
ppResolvedTypeDefault rtp =
  case rtp of
    ScalarType tp -> ppScalarDefault 0 tp
    MessageType _ -> text "mempty"
    EnumType e -> case Seq.viewl (e^.enumValues) of
                    Seq.EmptyL -> error "illegal: Enumerator constains no elements."
                    h Seq.:< _ -> ppText h

resolveFileDefType :: (Functor m, Monad m) => FileDef -> m ResolvedType
resolveFileDefType (EnumDef e) = return (EnumType e)
resolveFileDefType (MessageDef m) = do
  let nm = show (messageName m)
  case toUpperFirstName nm of
    Just t -> return (MessageType t)
    Nothing -> fail $ "Could not resolve message " ++ nm ++ " as a Haskell constructor."

resolveFieldType :: (Functor m, Monad m)
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

data ResolvedFieldInfo = RFI { rfiIdent :: !Ident
                             , rfiRule :: !A.FieldRule
                             , rfiType :: !ResolvedType
                             , rfiTag  :: !A.NumLit
                             , rfiDefault :: !(Maybe ResolvedVal)
                             , rfiIsPacked :: !Bool
                             }


resolveFieldInfo :: (Functor m, Monad m)
                 => TypeContext
                 -> FieldInfo
                 -> m ResolvedFieldInfo
resolveFieldInfo ctx f = do
  -- TODO: Check name is a lens name.
  tp <- resolveFieldType ctx (fieldType f)
  rv <- T.mapM (resolveValue ctx) (f^.fieldDefault)
  return RFI { rfiIdent = fieldName f
             , rfiRule = fieldRule f
             , rfiType = tp
             , rfiTag  = fieldTag f
             , rfiDefault = rv
             , rfiIsPacked = f^.fieldIsPacked
             }

-- | Return name of field as a stirng.
rfiStringName :: ResolvedFieldInfo -> String
rfiStringName f =
    case rfiRule f of
      A.Repeated -> nm ++ "s"
      _ -> nm
  where nm = show (rfiIdent f)

-- | Get name of lens associated with field.
rfiLensName :: ResolvedFieldInfo -> Doc
rfiLensName f =
  case toLowerFirstName (rfiStringName f) of
    Nothing -> error "Could not interpret field name as lens."
    Just s -> text s

rfiRecordFieldName :: ResolvedFieldInfo -> Doc
rfiRecordFieldName f = text "_" <> rfiLensName f

ppRfiType :: Int -> ResolvedFieldInfo -> Doc
ppRfiType prec f =
  case rfiRule f of
    A.Repeated -> parensIf (prec >= 10) (text "Seq" <+> ppFieldType (rfiType f))
    _ -> ppFieldType (rfiType f)

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

ppFieldDecl :: ResolvedFieldInfo -> Doc
ppFieldDecl f = rfiRecordFieldName f <+> text "::" <+> ppRfiType 0 f

-- | Print the default value to initialzie a field with.
ppFieldDefault :: ResolvedFieldInfo -> Doc
ppFieldDefault f | Just v <- rfiDefault f = pretty v
                 | otherwise = ppResolvedTypeDefault (rfiType f)

ppFieldInit :: ResolvedFieldInfo -> Doc
ppFieldInit rfi =
   rfiRecordFieldName rfi <+> text "=" <+> ppFieldDefault rfi

ppFieldLens :: Doc -> ResolvedFieldInfo -> Doc
ppFieldLens messageType f =
    l_nm <+> text ":: Simple Lens" <+> messageType <+> ppRfiType 10 f <$$>
    l_nm <+> text "= lens" <+> rec_nm <+> setter <$$>
    line
 where l_nm = rfiLensName f
       rec_nm = rfiRecordFieldName f
       setter = text "(\\s v -> s {" <+> rec_nm <+> text "= v })"

ppFieldAppend :: String -> String -> ResolvedFieldInfo -> Doc
ppFieldAppend x y f = rfiRecordFieldName f <+> text "=" <+> rhs
  where rhs | shouldMergeFields f =
              rfiRecordFieldName f <+> text x
                <+> text "<>"
                <+> rfiRecordFieldName f <+> text y
            | otherwise = rfiRecordFieldName f <+> text y



ppFieldDef :: ResolvedFieldInfo -> Doc
ppFieldDef f = do
  let f_nm = parens (text "fromString" <+> text (show (show (rfiIdent f))))
  let packed = text (if rfiIsPacked f then "Packed" else "Unpacked")
  let tag = text (show (rfiTag f))
  let req = case rfiRule f of
              A.Required -> text "Req"
              A.Optional -> parens (text "Opt" <+> ppFieldDefault f)
              _ -> error "internal: Default value not used in repeated fields."
  let lens_nm = rfiLensName f
  case rfiType f of
    ScalarType tp -> do
      let pre = case tp of
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
           text (pre ++ "RepeatedField") <+> f_nm <> showPacked <+> tag <+> lens_nm
        _ -> text (pre ++ "Field") <+> f_nm <+> req <+> tag <+> lens_nm
    MessageType nm -> do
      case rfiRule f of
        A.Required -> text "messageField messageRep" <+> f_nm <+> text "True"  <+> tag <+> lens_nm
        A.Optional -> text "messageField messageRep" <+> f_nm <+> text "False" <+> tag <+> lens_nm
        A.Repeated -> text "messageRepeatedField messageRep" <+> f_nm <+> tag <+> lens_nm
    EnumType e -> do
      let vals = enumLegalValues e
      case rfiRule f of
        A.Repeated -> text "enumRepeatedField" <+> f_nm <+> packed <+> vals <+> tag <+> lens_nm
        _ -> text "enumField" <+> f_nm <+> req <+> vals <+> tag <+> lens_nm



shouldMergeFields :: ResolvedFieldInfo -> Bool
shouldMergeFields f
  | A.Repeated <- rfiRule f = True
  | MessageType _ <- rfiType f = True
  | otherwise = False

------------------------------------------------------------------------
-- ResolvedMessage

data ResolvedMessage
   = RM { rmName :: Ident
        , rmFields :: [ResolvedFieldInfo]
        }

resolveMessage :: (Functor m, Monad m) => TypeContext -> MessageInfo -> m ResolvedMessage
resolveMessage ctx m = do
  fields <- mapM (resolveFieldInfo ctx) (Fold.toList (m^.messageFields))
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
    text "instance Monoid" <+> pretty nm <+> text "where" <$$>
    text "  mempty =" <+> pretty nm <+> text "{" <$$>
    indent 12 (vcatPrefix1 (text "  ") (text ", ") (ppFieldInit <$> fields)) <>
    text "}" <$$>
    text "  mappend x y =" <+> pretty nm <+> text "{" <$$>
    indent 17 (vcatPrefix1 (text "  ") (text ", ") (ppFieldAppend "x" "y" <$> fields)) <>
    text "}" <$$>
    text "" <$$>
    hcat (ppFieldLens (pretty nm) <$> fields) <>
    text "instance HasMessageRep" <+> pretty nm <+> text "where" <$$>
    text "  messageRep" <$$>
    text "    = emptyMessageRep (fromString" <+> nm_as_string <> text ")" <$$>
    indent 4 (vcat ((\f -> text "&" <+> ppFieldDef f) <$> fields)) <$$>
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
    A.BoolType     -> text "False"
    A.StringType   -> parensIf (prec >= 10) (text "fromString \"\"")
    A.BytesType    -> text "mempty"

------------------------------------------------------------------------
-- ResolvedDef

data ResolvedDef
   = ResolvedEnum EnumInfo
   | ResolvedMessage ResolvedMessage

instance Pretty ResolvedDef where
  pretty (ResolvedEnum e) = ppEnumInfo e
  pretty (ResolvedMessage m) = ppResolvedMessage m

resolveFileDef :: (Functor m, Monad m) => TypeContext -> FileDef -> m ResolvedDef
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
    text "module" <+> pretty (haskellModuleName pkg) <+> text "where" <$$>
    text "import Data.HPB" <$$>
    text "import Prelude ()" <$$>
    text "" <$$>
    vcat (pretty <$> moduleDefs pkg)

resolvePackage :: FilePath -> A.Package -> Either String Package
resolvePackage path pkg = runIdentity $ runErrorT $ do
  ctx <- mkFileContext pkg
  let nm = fromMaybe (moduleNameFromPath path) (ctx^.fcModuleName)
  let tpCtx = mkTypeContext ctx
  defs <- mapM (resolveFileDef tpCtx) (Fold.toList (ctx^.fcDefs))
  return Package { haskellModuleName = nm
                 , moduleDefs = defs
                 }