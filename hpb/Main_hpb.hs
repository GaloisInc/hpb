module Main (main) where

import Control.Lens
import Control.Exception
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.Foldable as Fold
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Version

import System.Console.CmdArgs.Explicit
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import qualified Text.PrettyPrint.Leijen as PP

import Data.HPB.Parser

import Paths_hpb (version)

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
  = Args { _argAction :: ArgAction
         , _protoFiles :: Seq FilePath
         }

-- | Initial arguments if nothing is specified.
defaultArgs :: Args
defaultArgs = Args { _argAction = GenerateCode
                   , _protoFiles = Seq.empty
                   }

argAction :: Simple Lens Args ArgAction
argAction = lens _argAction (\a v -> a { _argAction = v })

protoFiles :: Simple Lens Args (Seq FilePath)
protoFiles = lens _protoFiles (\a v -> a { _protoFiles = v })

filenameArg :: Arg Args
filenameArg = Arg { argValue = addFilename
                  , argType = "PROTO_FILES"
                  , argRequire = False
                  }
  where addFilename :: String -> Args -> Either String Args
        addFilename nm a = Right (a & protoFiles %~ (Seq.|> nm))

parseFlag :: Flag Args
parseFlag = flagNone [ "parse", "p" ] upd help
  where upd  = argAction .~ ShowParse
        help = "Parse PROTO_FILES files and show output for debugging purposes."

arguments :: Mode Args
arguments = mode "hpb" defaultArgs help filenameArg flags
  where help = hpbVersion
        flags = [ parseFlag
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
showParse args =
  Fold.forM_ (args^.protoFiles) $ \path -> do
    decls <- loadAndParseFile path `catch` exitOnError
    PP.displayIO stdout $ PP.renderPretty 1.0 maxBound $ ppDecls decls
    putStrLn ""

printErrorAndExit :: String -> IO a
printErrorAndExit msg = do
  hPutStrLn stderr msg
  exitFailure

exitOnError :: IOError -> IO a
exitOnError e = printErrorAndExit (ioeGetErrorString e)

loadAndParseFile :: FilePath -> IO [Decl]
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
        Left msg -> do
          fail $ "Error parsing " ++ path ++ ":\n"
                  ++ show (PP.indent 2 (PP.text msg))

main :: IO ()
main = do
  -- TODO Parse arguments.
  args <- getCommandLineArgs
  case args^.argAction of
    GenerateCode -> do
      error "GenerateCode undefined"
      -- Write generated code to output.
    ShowParse -> showParse args `catch` exitOnError
      -- Parse proto file.
    ShowHelp -> do
      print $ helpText [] HelpFormatDefault arguments
    ShowVersion -> do
      putStrLn (modeHelp arguments)
      putStrLn $ "\n" ++ copyrightLong