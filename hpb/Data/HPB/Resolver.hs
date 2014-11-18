{-# LANGUAGE OverloadedStrings #-}
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
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import System.FilePath
import Text.PrettyPrint.Leijen as PP hiding ((<$>), line)

import Data.HPB.AST (Ident)
import qualified Data.HPB.AST as A
import qualified Data.HPB.Parser as P

ppText :: Text -> Doc
ppText t = text (Text.unpack t)

isUpperRoman :: Char -> Bool
isUpperRoman c = 'A' <= c && c <= 'Z'

isLowerRoman :: Char -> Bool
isLowerRoman c = 'a' <= c && c <= 'z'

------------------------------------------------------------------------
-- ModuleName

data ModuleName = ModuleName [Text]

instance Pretty ModuleName where
  pretty (ModuleName l) = hcat (punctuate dot (ppText <$> l))

fixModuleNameChars :: String -> Maybe Text
fixModuleNameChars s = Text.pack <$> fixChars s
  where fixChars [] = Nothing
        fixChars (c:r) | isUpperRoman c = Just (c : map fixRest r)
                       | isLowerRoman c = Just (toUpper c : map fixRest r)
                       | otherwise = fixChars r
        isGood c | isUpperRoman c = True
                 | isLowerRoman c = True
                 | isDigit c = True
                 | c == '_' = True
                 | otherwise = False
        fixRest c | isGood c = c
                  | otherwise = '_'

moduleNameFromPackageName :: A.CompoundName -> Maybe ModuleName
moduleNameFromPackageName (A.CompoundName l) | null r = Nothing
                                             | otherwise = Just (ModuleName r)
  where r = mapMaybe f l
        f nm = fixModuleNameChars (show (A.val nm))

moduleNameFromPath :: FilePath -> ModuleName
moduleNameFromPath path = ModuleName [fromMaybe "Main" (fixModuleNameChars nm)]
  where nm = dropExtension $ takeFileName path

------------------------------------------------------------------------
-- Uncategorized

type PackageName = A.CompoundName


type HaskellModuleName = Text




data Package = Package { haskellModuleName :: !ModuleName
                       , moduleMessages :: ![Message]
                       , moduleEnum :: ![EnumInfo]
                       }

instance Pretty Package where
  pretty pkg =
    text "module" <+> pretty (haskellModuleName pkg) <+> text "where" <$$>
    text "import Data.HPB" <$$>
    text "import Prelude (Enum(..), error, show, (++))" <$$>
    vcat (ppMessageDecl <$> moduleMessages pkg) <$$>
    vcat (ppEnumInfo <$> moduleEnum pkg)


type FullyQualifiedMessageName = A.CompoundName

data Message = Message { messageCtor :: !Text
                       , messageFields :: ![Field]
                       }

data FieldType
   = ScalarFieldType A.ScalarType
   | MessageFieldType FullyQualifiedMessageName

ppFieldType :: FieldType -> Doc
ppFieldType = undefined

data Field = Field { fieldName :: Ident
                   , fieldType :: FieldType
                   }

ppMessageDecl :: Message -> Doc
ppMessageDecl m =
  text "data" <+> ppText (messageCtor m) <$$>
  text "   =" <+> ppText (messageCtor m) <+> text "{" <$$>
  vcatPrefix1 (text "     ")
              (text "   , ")
              (ppFieldRecord m <$> messageFields m) <$$>
  text "   }"

vcatPrefix1 :: Doc -> Doc -> [Doc] -> Doc
vcatPrefix1 _ _ [] = PP.empty
vcatPrefix1 ph pr (h:r) = vcat ((ph <> h) : fmap (pr <>) r)

ppMessageFields :: Message -> Doc
ppMessageFields m = undefined m

-- | Print out the name of the haskell field for this record.
fieldRecordField :: Message -> Field -> Doc
fieldRecordField = undefined

ppFieldRecord :: Message -> Field -> Doc
ppFieldRecord m f = fieldRecordField m f <+> text "::" <+> ppFieldType (fieldType f)

------------------------------------------------------------------------
-- EnumInfo

data EnumInfo
   = EnumInfo { -- | Name used for haskell data type
                enumHaskellName :: Text
              , enumPos :: A.SourcePos
              , _enumIdentMap :: Map Ident A.SourcePos
              , _enumCtors  :: Map Ident Word32
              , _enumValues :: Map Word32 [Ident]
              , _enumAllowAlias :: Maybe Bool
              }

-- | Map each identifier to it's known location.
enumIdentMap :: Simple Lens EnumInfo (Map Ident A.SourcePos)
enumIdentMap = lens _enumIdentMap (\s v -> s { _enumIdentMap = v })

-- | Map each identifier used as a constructor to it's value.
enumCtors :: Simple Lens EnumInfo (Map Ident Word32)
enumCtors = lens _enumCtors (\s v -> s { _enumCtors = v })

-- | Map each enum value to an associated identifier.
enumValues :: Simple Lens EnumInfo (Map Word32 [Ident])
enumValues = lens _enumValues (\s v -> s { _enumValues = v })

-- | Flag indicating if enum allows duplicates.
enumAllowAlias :: Simple Lens EnumInfo (Maybe Bool)
enumAllowAlias = lens _enumAllowAlias (\s v -> s { _enumAllowAlias = v })

enumHaskellType :: EnumInfo -> Doc
enumHaskellType = ppText . enumHaskellName

ppToEnumBindings :: EnumInfo -> Doc
ppToEnumBindings e = vcat (bindings ++ [end])
  where bindings = ppToEnumBinding <$> Map.toList (e^.enumValues)
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
  indent 3 (vcatPrefix1 (text "= ") (text "| ") (pretty <$> Map.keys (e^.enumCtors))) <$$>
  text "" <$$>
  text "instance Enum" <+> enumHaskellType e <+> text "where" <$$>
  indent 2 (ppToEnumBindings e) <$$>
  indent 2 (vcat (ppFromEnumBinding <$> Map.toList (e^.enumCtors))) <$$>
  text ""



type EnumResolver = StateT EnumInfo (ErrorT String Identity)

asBoolVal :: Monad m => A.SourcePos -> String -> A.Val -> m Bool
asBoolVal _ _ (A.BoolVal b) = return b
asBoolVal p msg _ = failAt p msg

resolveEnumField :: A.EnumField -> EnumResolver ()
resolveEnumField (A.EnumOption (A.OptionDecl (A.Posd (A.KnownName "allow_alias") p) v)) = do
  old_val <- use enumAllowAlias
  when (isJust old_val) $ do
    failAt p $ "allow_alias already set."
  b <- asBoolVal p "allow_alias expected Boolean value." v
  enumAllowAlias .= Just b
resolveEnumField (A.EnumOption _) = do
  return ()
resolveEnumField (A.EnumValue (A.Posd nm p) v) = do
  im <- use enumIdentMap
  case Map.lookup nm im of
    Just old_p -> failAt p $ show nm ++ " already declared at " ++ show old_p ++ "."
    Nothing -> return ()
  vm <- use enumValues
  let w = fromIntegral (A.numVal v)

  enumIdentMap %= Map.insert nm p
  case Map.lookup w vm of
    Nothing -> enumCtors %= Map.insert nm w
    Just{} -> return ()
  enumValues %= Map.insertWith (++) w [nm]



------------------------------------------------------------------------
-- FileContext

data FileContext = FCtx { _fcModuleName :: !(Maybe ModuleName)
                        , _fcImports    :: !(Map A.StringLit (A.SourcePos, A.ImportVis))
                        , _fcEnums      :: !(Map Ident EnumInfo)
                        , _fcMessages   :: !(Map Ident A.MessageDecl)
                        , _fcCtorIdents :: !(Set Ident)
                        }

--
fcModuleName :: Simple Lens FileContext (Maybe ModuleName)
fcModuleName = lens _fcModuleName (\s v -> s { _fcModuleName = v })

fcImports :: Simple Lens FileContext (Map A.StringLit (A.SourcePos, A.ImportVis))
fcImports = lens _fcImports (\s v -> s { _fcImports = v })

fcEnums :: Simple Lens FileContext (Map Ident EnumInfo)
fcEnums = lens _fcEnums (\s v -> s { _fcEnums = v })

fcMessages :: Simple Lens FileContext (Map Ident A.MessageDecl)
fcMessages = lens _fcMessages (\s v -> s { _fcMessages = v })

fcCtorIdents :: Simple Lens FileContext (Set Ident)
fcCtorIdents = lens _fcCtorIdents (\s v -> s { _fcCtorIdents = v })

emptyFileContext :: FileContext
emptyFileContext  =
  FCtx { _fcModuleName = Nothing
       , _fcImports = Map.empty
       , _fcEnums = Map.empty
       , _fcMessages = Map.empty
       , _fcCtorIdents = Set.empty
       }

type FileResolver = StateT FileContext (ErrorT String Identity)

resolveFileOption :: A.SourcePos -> A.OptionName -> A.Val -> FileResolver ()
resolveFileOption _ _ _ = return ()

failAt :: Monad m => A.SourcePos -> String -> m a
failAt p msg = fail $ show $
  pretty p <> text ":" <$$>
  indent 2 (text msg)

resolveEnum :: A.EnumDecl -> FileResolver ()
resolveEnum d = do
  let A.Posd nm p = A.enumIdent d
  m <- use fcEnums
  case Map.lookup nm m of
    Nothing -> return ()
    Just e -> fail $ "Enumeration already defined at " ++ show (enumPos e)
  let e0 = EnumInfo { enumHaskellName = A.identText nm
                    , enumPos = p
                    , _enumIdentMap = Map.empty
                    , _enumCtors = Map.empty
                    , _enumValues = Map.empty
                    , _enumAllowAlias = Nothing
                    }
  e <- lift $ flip execStateT e0 $ do
         mapM_ resolveEnumField (A.enumFields d)
  when (Map.null (e^.enumCtors)) $ do
    failAt p $ "Enumerations must contain at least one value."
  when (e^.enumAllowAlias /= Just True) $ do
    let isDup (nm, l) = length l > 1
    case filter isDup $ Map.toList (e^.enumValues) of
      [] -> return ()
      _ -> fail $ "Enumeration contains aliases, but allow_alias is not set."
  fcEnums . at nm .= Just e

resolveFileDecl :: A.Decl -> FileResolver ()
resolveFileDecl decl = do
  case decl of
    A.Import _ _ -> do
      fail "hdb does not yet support imports."
    A.Option (A.OptionDecl (A.Posd nm p) v) ->
      resolveFileOption p nm v
    A.Enum d -> resolveEnum d
    A.Message _  ->
      return ()
    A.Extend _   -> fail "hdb does not yet support message extensions."
    A.Service _  -> fail "hdb does not yet support service declarations."



mkFileContext :: A.Package -> ErrorT String Identity FileContext
mkFileContext (A.Package pkg_nm decls) = execStateT go emptyFileContext
  where go = do
          mapM_ resolveFileDecl decls
          -- Assign module name if not defined.
          mdef <- use fcModuleName
          case mdef of
            Just{} -> return ()
            Nothing -> fcModuleName .= (moduleNameFromPackageName =<< pkg_nm)

resolvePackage :: FilePath -> A.Package -> Either String Package
resolvePackage path pkg = runIdentity $ runErrorT $ do
  ctx <- mkFileContext pkg
  let nm = fromMaybe (moduleNameFromPath path) (ctx^.fcModuleName)
  return Package { haskellModuleName = nm
                 , moduleMessages = []
                 , moduleEnum = Map.elems (ctx^.fcEnums)
                 }

{-
data ResolverState
   = RS { _rsModuleName :: !ModuleName
        , _rsExplicitModule :: !Bool
        , _rsMessages :: !(Seq A.MessageDecl)
        , _rsEnums :: !(Seq EnumInfo)
        }

rsModuleName :: Simple Lens ResolverState ModuleName
rsModuleName = lens _rsModuleName (\s v -> s { _rsModuleName = v })

rsExplicitModule :: Simple Lens ResolverState Bool
rsExplicitModule = lens _rsExplicitModule (\s v -> s { _rsExplicitModule = v })

rsMessages :: Simple Lens ResolverState (Seq MessageDecl)
rsMessages = lens _rsMessages (\s v -> s { _rsMessages = v })

rsEnums :: Simple Lens ResolveState (Seq EnumInfo)
rsEnums = lens _rsEnums (\s v -> s { _rsEnums = v })

initState :: FilePath -> ResolverState
initState path =
  RS { _rsModuleName = moduleNameFromPath path
     , _rsExplicitModule = False
     , _rsMessages = Seq.empty
     }

mkPackage :: ResolverState -> Package
mkPackage s = Package { haskellModuleName =  s^.rsModuleName
                      , moduleMessages = Fold.toList (s^.rsMessages)
                      }

type Resolver = StateT ResolverState (ErrorT String Identity)

resolveDecl :: A.Decl -> Resolver ()
resolveDecl d =
  case d of
    A.PackageName nm -> do
      case moduleNameFromPackageName nm of
        Just pkg_nm -> do
          def <- use rsExplicitModule
          when (not def) $ do
            rsModuleName .= pkg_nm
        Nothing -> return ()
    A.Import _ _ -> fail "hdb does not yet support imports."
    A.Option (A.OptionDecl (A.Posd nm p) v) ->
      resolveFileOption p nm v
    A.Enum d     ->
      resolveEnum d
    A.Message _  ->
      return ()
    A.Extend _   -> fail "hdb does not yet support message extensions."
    A.Service _  -> fail "hdb does not yet support service declarations."

resolvePackage :: FilePath -> [A.Decl] -> Either String Package
resolvePackage path decls = fmap mkPackage $ runIdentity $ runErrorT $ execStateT m s
  where m = mapM_ resolveDecl decls
        s = initState path
-}
