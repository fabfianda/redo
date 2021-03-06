{-# LANGUAGE ScopedTypeVariables #-}

module Redo
  ( main,
  )
where

import Control.Exception (IOException, catch, catchJust, throw)
import Control.Monad (filterM, guard, liftM, unless)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy as BL (readFile)
import Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Digest.Pure.MD5 as MD5 (md5)
import Data.Map.Lazy (Map (..))
import qualified Data.Map.Lazy as M (fromList, insert, lookup, toList)
import Data.Maybe (listToMaybe)
import Debug.Trace (traceShowId)
import GHC.IO.Exception (IOErrorType (..), IOException)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getDirectoryContents, removeDirectoryRecursive, removeFile, renameFile, setCurrentDirectory)
import System.Environment (getArgs, getEnvironment, getProgName, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>), hasExtension, replaceBaseName, splitFileName, takeBaseName)
import System.IO (IOMode (..), hFileSize, hGetLine, hPrint, hPutStrLn, stderr, withFile)
import System.IO.Error (ioeGetErrorType, isDoesNotExistError)
import System.Process (CreateProcess (env), createProcess, shell, waitForProcess)

-- constants
cfgMetaDir = ".redo" :: FilePath

-- types
type Target = String

type Dependency = String

-- main
main :: IO ()
main = do
  hPutStrLn stderr "Redo v0.1"
  topLevelDir <- getCurrentDirectory
  mapM_ (redoInTargetDir topLevelDir) =<< getArgs
  progName <- getProgName
  redoTarget <- lookupEnv "REDO_TARGET"
  case (progName, redoTarget) of
    ("redo-ifchange", Just redoTarget) ->
      mapM_ (writeMD5 redoTarget) =<< getArgs
    ("redo-ifchange", Nothing) -> error "Missing REDO_TARGET"
    _ -> return ()
  where
    redoInTargetDir :: FilePath -> FilePath -> IO ()
    redoInTargetDir topLevelDir path = do
      let (dir, target) = splitFileName path
      setCurrentDirectory dir
      redo target
      setCurrentDirectory topLevelDir

--
setupDir :: FilePath -> IO ()
setupDir path = do
  -- clear dir
  catchJust
    (guard . isDoesNotExistError)
    (removeDirectoryRecursive path)
    return
  -- recreate it
  createDirectoryIfMissing True path

--
redo :: Target -> IO ()
redo target = do
  targetIsUpToDate <- isUpToDate target
  unless
    targetIsUpToDate
    (runDo target)

runDo :: Target -> IO ()
runDo target = getTargetDoFilePath target >>= maybe doMissing run
  where
    run :: FilePath -> IO ()
    run doFile = do
      hPutStrLn stderr $ "redo: " ++ target
      -- clean and create meta dir
      setupDir $ cfgMetaDir </> target
      -- write the do file hash
      writeMD5 target doFile
      -- read env
      currentEnvMap <- M.fromList <$> getEnvironment
      let maybeEnvPath = getMaybeEnvPath currentEnvMap
      -- add current dir to path
      let envPath = maybe "." (++ ":.") maybeEnvPath
      let envMap =
            updateEnvMap ("PATH", envPath) $
              updateEnvMap ("REDO_TARGET", target) currentEnvMap
      -- run the do file
      (_, _, _, processHandle) <- createProcess $ (shell $ cmd doFile) {env = Just (M.toList envMap)}
      --
      exitCode <- waitForProcess processHandle
      case exitCode of
        ExitSuccess -> do
          tmpFileSize <- fileSize tmpFile
          if tmpFileSize > 1 then renameFile tmpFile target else removeFile tmpFile
        ExitFailure e -> do
          removeFile tmpFile
          hPutStrLn stderr $ "Build failed with error code: " ++ show e
          exitWith exitCode
    -- what if do file is missing?
    doMissing :: IO ()
    doMissing = do
      fileExists <- doesFileExist target
      unless fileExists (error $ "Target " ++ target ++ " does not exist")
    -- temporary file for atomic replace
    tmpFile :: String
    tmpFile = target ++ "--redoing"
    -- build the shell command
    cmd :: FilePath -> String
    cmd path = unwords ["sh -x", path, "0", takeBaseName target, tmpFile, ">", tmpFile]

-- getTargetDoFilePath - Look for <target>.do or default.do.<ext>
getTargetDoFilePath :: Target -> IO (Maybe FilePath)
getTargetDoFilePath target = listToMaybe <$> filterM doesFileExist candidates
  where
    candidates = [target ++ ".do"] <> [replaceBaseName target "default" ++ ".do" | hasExtension target]

-- isUpToDate
isUpToDate :: Target -> IO Bool
isUpToDate target =
  catch
    (isUpToDate' target)
    -- if .redo folder is missing then target cannot be up to date
    (\(_ :: IOException) -> return False)

isUpToDate' :: Target -> IO Bool
isUpToDate' target = do
  fileExists <- doesFileExist target
  if fileExists
    then do
      deps <- getDirectoryContents (cfgMetaDir </> target)
      and <$> mapM isDepUpToDate deps
    else return False
  where
    isDepUpToDate :: FilePath -> IO Bool
    isDepUpToDate dep =
      catch
        ( do
            (depMD5, depFileName) <- readDepMD5 target dep
            newMD5 <- fileMD5 depFileName
            let isDepSame = depMD5 == newMD5
            depDoFile <- getTargetDoFilePath dep
            maybe
              (return isDepSame)
              ( \_ -> do
                  isDepDoTargetUpToDate <- isUpToDate dep
                  return (isDepSame && isDepDoTargetUpToDate)
              )
              depDoFile
        )
        -- if dep file is not readable (e.g : . , .. )
        ( \e -> return (ioeGetErrorType e == InappropriateType)
        )

-- Helpers

-- EnvMap helpers

type EnvMap = Map String String

updateEnvMap :: (String, String) -> EnvMap -> EnvMap
updateEnvMap (x, y) = M.insert x y

type EnvPath = String

getMaybeEnvPath :: EnvMap -> Maybe EnvPath
getMaybeEnvPath = M.lookup "PATH"

-- MD5 helpers
-- we use MD5 on the dep name too to avoid filesystem clashes
writeMD5 :: Target -> Dependency -> IO ()
writeMD5 target dep = do
  md5 <- fileMD5 dep
  let lines = md5 ++ "\n" ++ dep
  writeFile (cfgMetaDir </> target </> depNameMD5 dep) lines

readDepMD5 :: Target -> Dependency -> IO (String, String)
readDepMD5 target dep =
  withFile
    (cfgMetaDir </> target </> dep)
    ReadMode
    ( \handle -> do
        md5 <- hGetLine handle
        filename <- hGetLine handle
        return (md5, filename)
    )

fileMD5 :: FilePath -> IO String
fileMD5 path = show . MD5.md5 <$> BL.readFile path

fileSize :: FilePath -> IO Integer
fileSize path = withFile path ReadMode hFileSize

depNameMD5 :: String -> String
depNameMD5 = toMD5

toMD5 :: String -> String
toMD5 = show . MD5.md5 . BLU.fromString
