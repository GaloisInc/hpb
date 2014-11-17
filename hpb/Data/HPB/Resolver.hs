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
                       }

instance Pretty Package where
  pretty pkg =
    text "module" <+> pretty (haskellModuleName pkg) <+> text "where" <$$>
    text "import Data.HPB" <$$>
    text "import Prelude ()" <$$>
    vcat (ppMessageDecl <$> moduleMessages pkg)


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
  ppMessageFields m <>
  text "   }"

ppMessageFields :: Message -> Doc
ppMessageFields m =
  case messageFields m of
    [] -> PP.empty
    (h:r) ->
       text "    " <> ppFieldRecord m h <$$>
       vcat ((\f -> text "  , " <> ppFieldRecord m f) <$> r)


-- | Print out the name of the haskell field for this record.
fieldRecordField :: Message -> Field -> Doc
fieldRecordField = undefined

ppFieldRecord :: Message -> Field -> Doc
ppFieldRecord m f = fieldRecordField m f <+> text "::" <+> ppFieldType (fieldType f)

data EnumInfo = EnumInfo { -- | Name used for haskell data type
                           enumHaskellName :: Text
                         , enumPos :: A.SourcePos
                         , enumHaskellValues :: [(Text, Word32)]
                         }

data FileContext = FCtx { _fcModuleName :: !(Maybe ModuleName)
                        , _fcImports    :: Map A.StringLit (A.SourcePos, A.ImportVis)
                        , _fcEnums      :: Map Ident EnumInfo
                        , _fcMessages   :: Map Ident A.MessageDecl
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

emptyFileContext :: FileContext
emptyFileContext =
  FCtx { _fcModuleName = Nothing
       , _fcImports = Map.empty
       , _fcEnums = Map.empty
       , _fcMessages = Map.empty
       }

type FileResolver = StateT FileContext (ErrorT String Identity)

resolveFileOption :: A.SourcePos -> A.OptionName -> A.Posd A.Val -> FileResolver ()
resolveFileOption _ _ _ = return ()

resolveFileDecl :: A.Decl -> FileResolver ()
resolveFileDecl d = do
  case d of
{-
    A.PackageName nm -> do
      case moduleNameFromPackageName nm of
        Just pkg_nm -> do
          mdef <- use fcModuleName
          case mdef of
            Nothing -> when (not def) $ do
            rsModuleName .= pkg_nm
        Nothing -> return ()
-}
    A.Import _ _ -> do
      fail "hdb does not yet support imports."
    A.Option (A.OptionDecl (A.Posd nm p) v) ->
      resolveFileOption p nm v
    A.Enum d -> do
      let A.Posd nm p = A.enumIdent d
      m <- use fcEnums
      case Map.lookup nm m of
        Nothing -> return ()
        Just e -> do
          fail $ "Enumeration already defined at " ++ show (enumPos e)

      fcEnums . at nm .= Just (undefined d)
    A.Message _  ->
      return ()
    A.Extend _   -> fail "hdb does not yet support message extensions."
    A.Service _  -> fail "hdb does not yet support service declarations."



mkFileContext :: [A.Decl] -> ErrorT String Identity FileContext
mkFileContext decls = execStateT (mapM_ resolve decls) emptyFileContext
  where resolve d = resolveFileDecl d


resolvePackage :: FilePath -> A.Package -> Either String Package
resolvePackage = undefined

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
