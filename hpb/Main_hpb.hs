{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Exception
import Control.Lens
import Control.Monad (when)
import qualified Data.ByteString.Lazy as LazyBS
import Data.Foldable (forM_)
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Version
import System.Console.CmdArgs.Explicit
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Data.HPB.AST as A
import Data.HPB.Parser
import Data.HPB.Resolver

import Paths_hpb (version)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

hpbVersion :: String
hpbVersion = "Haskell Protocol Buffers Generator (hpb) "
             ++ versionString
  where [h,l,r] = versionBranch version
        versionString = show h ++ "." ++ show l ++ "." ++ show r

copyrightShort :: String
copyrightShort = "Copyright 2014 Joe Hendrix."

copyrightLong :: String
copyrightLong= unlines
  [ copyrightShort
  , ""
  , "Licensed under the Apache License, Version 2.0 (the \"License\");"
  , "you may not use this file except in compliance with the License."
  , "You may obtain a copy of the License at"
  , ""
  , "  http://www.apache.org/licenses/LICENSE-2.0"
  , ""
  , "Unless required by applicable law or agreed to in writing, software"
  , "distributed under the License is distributed on an \"AS IS\" BASIS,"
  , "WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."
  , "See the License for the specific language governing permissions and"
  , "limitations under the License."
  ]

data ArgAction
   = GenerateCode
   | ShowParse
   | ShowHelp
   | ShowVersion

data Args
  = Args { _argAction :: !ArgAction
         , _protoFiles :: !(Seq FilePath)
         , _importDirs :: !(Seq FilePath)
         , _outDir     :: !(Maybe FilePath)
         , _moduleName :: !(Maybe String)
         }

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _argAction  = GenerateCode
                   , _protoFiles = Seq.empty
                   , _importDirs = Seq.empty
                   , _outDir     = Nothing
                   , _moduleName = Nothing
                   }

-- | Action to use for assigning.
argAction :: Lens' Args ArgAction
argAction = lens _argAction (\a v -> a { _argAction = v })

-- | List of paths to output.
protoFiles :: Lens' Args (Seq FilePath)
protoFiles = lens _protoFiles (\a v -> a { _protoFiles = v })

-- | List of directories to use for searching for imports.
importDirs :: Lens' Args (Seq FilePath)
importDirs = lens _importDirs (\a v -> a { _importDirs = v })

-- | List of directories to use for searching for imports.
outDir :: Lens' Args (Maybe FilePath)
outDir = lens _outDir (\a v -> a { _outDir = v })

-- | List of directories to use for searching for imports.
moduleName :: Lens' Args (Maybe String)
moduleName = lens _moduleName (\a v -> a { _moduleName = v })

filenameArg :: Arg Args
filenameArg = Arg { argValue = addFilename
                  , argType = "PROTO_FILES"
                  , argRequire = False
                  }
  where addFilename :: String -> Args -> Either String Args
        addFilename nm a = Right (a & protoFiles %~ (Seq.|> nm))

parseFlag :: Flag Args
parseFlag = flagNone [ "parse", "P" ] upd help
  where upd  = argAction .~ ShowParse
        help = "Parse PROTO_FILES files and show output for debugging purposes."

importFlag :: Flag Args
importFlag = flagReq [ "proto_path", "I" ] upd "PATH" help
  where upd path a = Right (a & importDirs %~ (Seq.|> path))
        help = "Specify directory to search for imports."

outDirFlag :: Flag Args
outDirFlag = flagReq ["out", "O" ] upd "OUT_DIR" help
  where upd path a = Right (a & outDir .~ Just path)
        help = "Specify where to write generated files."

moduleNameFlag :: Flag Args
moduleNameFlag = flagReq ["module" ] upd "MODULE_NAME" help
  where upd nm a = Right (a & moduleName .~ Just nm)
        help = "Specify name of module to generate."

arguments :: Mode Args
arguments = mode "hpb" defaultArgs help filenameArg flags
  where help = hpbVersion
        flags = [ parseFlag
                , importFlag
                , outDirFlag
                , moduleNameFlag
                , flagHelpSimple (argAction .~ ShowHelp)
                , flagVersion (argAction .~ ShowVersion)
                ]

getCommandLineArgs :: IO Args
getCommandLineArgs = do
  argStrings <- getArgs
  case process arguments argStrings of
    Left msg -> do
      hPutStrLn stderr msg
      exitFailure
    Right v -> return v

showParse :: Args -> IO ()
showParse args = do
  when (Seq.null (args^.protoFiles)) $ do
    fail $ "Please provide a proto file as input."
  forM_ (args^.protoFiles) $ \path -> do
    p <- loadAndParseFile path
    PP.displayIO stdout $ PP.renderPretty 1.0 maxBound $ PP.pretty p
    putStrLn ""

printErrorAndExit :: String -> IO a
printErrorAndExit msg = do
  hPutStrLn stderr msg
  exitFailure

exitOnError :: IOError -> IO a
exitOnError e = printErrorAndExit (ioeGetErrorString e)

loadAndParseFile :: FilePath -> IO A.Package
loadAndParseFile path = do
  mcontents <- try $ LazyBS.readFile path
  case mcontents of
    Left e
     | isDoesNotExistError e ->
       fail $ "File not found: " ++ path
     | isPermissionError e -> do
       fail $ "Insufficient permissions to read " ++ path ++ "."
     | otherwise -> throwIO e
    Right contents -> do
      case parseDecls path contents of
        Right m -> return m
        Left (p,msg) -> do
          fail $ show $
            PP.text "Parse error" PP.<+> PP.pretty p PP.<> PP.text ":" PP.<$$>
            PP.indent 2 (PP.text msg)


getFileName :: FilePath -> ModuleName -> IO (FilePath,FilePath)
getFileName dir0 (ModuleName l0)
  | null l0 = fail "Unexpected empty module name."
  | otherwise = do
      let l = Text.unpack <$> l0
      let dir = joinPath (dir0 : init l)
      let f = last l
      return (dir,f ++ ".hs")

generateCode :: Args -> IO ()
generateCode args = do
  when (Seq.null (args^.protoFiles)) $ do
    fail $ "Please provide a proto file as input."
  forM_ (args^.protoFiles) $ \path -> do
    pkg <- loadAndParseFile path
    case resolvePackage path (args^.moduleName) pkg of
      Left msg -> fail msg
      Right mdl -> do
        (dir,f) <- getFileName (fromMaybe "." (args^.outDir)) (haskellModuleName mdl)
        createDirectoryIfMissing True dir
        writeFile (dir </> f) (show (PP.pretty mdl))

main :: IO ()
main = do
  args <- getCommandLineArgs
  case args^.argAction of
    GenerateCode -> generateCode args `catch` exitOnError
    ShowParse -> showParse args `catch` exitOnError
      -- Parse proto file.
    ShowHelp -> do
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion -> do
      putStrLn (modeHelp arguments)
      putStrLn $ "\n" ++ copyrightLong