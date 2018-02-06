module RenameModule where

import           Prelude hiding (readFile)

import           Control.Monad
import           Data.List
import           Data.Char
import           System.FilePath
import           System.Directory
import           System.Process
import           System.IO.Strict

type ModuleName = [String]

showModuleName :: ModuleName -> String
showModuleName = intercalate "."

rename :: FilePath -> FilePath -> IO ()
rename = renameWith renameFile

gitRename :: FilePath -> FilePath -> IO ()
gitRename = renameWith gitMove
  where
    gitMove :: FilePath -> FilePath -> IO ()
    gitMove src dst = callProcess "git" ["mv", src, dst]

renameWith :: (FilePath -> FilePath -> IO ()) -> FilePath -> FilePath -> IO ()
renameWith move src dstDirOrFile = do
  isDir <- doesDirectoryExist dstDirOrFile
  let
    dst = if isDir
      then dstDirOrFile </> takeFileName src
      else case splitFileName dstDirOrFile of
        (dir, "") -> dir </> takeFileName src
        _ -> dstDirOrFile
  renameModuleWith move src dst
  hasSpec <- doesFileExist $ specFile src
  when hasSpec $ do
    renameModuleWith move (specFile src) (specFile dst)

renameModuleWith :: (FilePath -> FilePath -> IO ()) -> FilePath -> FilePath -> IO ()
renameModuleWith move src dst = do
  createDirectoryIfMissing True (takeDirectory dst)
  move src dst
  readFile dst >>= writeFile dst . renameModule (moduleName src) (moduleName dst)

renameModule :: ModuleName -> ModuleName -> String -> String
renameModule src dst input = case stripPrefix (showModuleName src) input of
  Just xs -> showModuleName dst ++ renameModule src dst xs
  Nothing -> case input of
    x : xs -> x : renameModule src dst xs
    [] -> []

specFile :: FilePath -> FilePath
specFile = filePath "test" . specFor . moduleName

specFor :: ModuleName -> ModuleName
specFor name = case reverse name of
  x : xs -> reverse ((x ++ "Spec") : xs)
  _ -> name

filePath :: FilePath -> ModuleName -> FilePath
filePath base name = joinPath (base : name) ++ ".hs"

moduleName :: FilePath -> ModuleName
moduleName = dropWhile (not . isValidModuleName) . splitDirectories . dropExtension

-- See `Cabal.Distribution.ModuleName` (http://git.io/bj34)
isValidModuleName :: String -> Bool
isValidModuleName [] = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''
