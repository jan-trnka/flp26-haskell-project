-- | Discovering @.test@ files and their companion @.in@\/@.out@ files.
module SOLTest.Discovery (discoverTests) where

import SOLTest.Types
import System.Directory
  ( doesFileExist,
    listDirectory,
    doesDirectoryExist,
  )
import System.FilePath (replaceExtension, takeBaseName, (</>), takeExtension)
import Control.Monad (forM)

-- | Discover all @.test@ files in a directory.
--
-- When @recursive@ is 'True', subdirectories are searched recursively.
-- Returns a list of 'TestCaseFile' records, one per @.test@ file found.
-- The list is ordered by the file system traversal order (not sorted).
discoverTests :: Bool -> FilePath -> IO [TestCaseFile]
discoverTests recursive dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries

  -- For each entry from @fullPaths@, check if it's a directory or a file.
  tcf <- forM fullPaths $ \path -> do
    -- If it's a directory, check if the discovery should be recursive.
    isDir <- doesDirectoryExist path
    if isDir
      then
        if recursive
          -- If @recursive@ is 'True', discover tests in this subdirectory.
          then discoverTests recursive path
          else return []
      else
        -- If it's a file, check if it has the @.test@ extension.
        if takeExtension path == ".test"
          then do
            -- Build a 'TestCaseFile' using 'findCompanionFiles' function.
            test <- findCompanionFiles path
            return [test]
          else return []

  -- Flatten the list of lists of 'TestCaseFile' into a single list and return it.
  return (concat tcf)

-- | Build a 'TestCaseFile' for a given @.test@ file path, checking for
-- companion @.in@ and @.out@ files in the same directory.
findCompanionFiles :: FilePath -> IO TestCaseFile
findCompanionFiles testPath = do
  let baseName = takeBaseName testPath
      inFile = replaceExtension testPath ".in"
      outFile = replaceExtension testPath ".out"
  hasIn <- doesFileExist inFile
  hasOut <- doesFileExist outFile
  return
    TestCaseFile
      { tcfName = baseName,
        tcfTestSourcePath = testPath,
        tcfStdinFile = if hasIn then Just inFile else Nothing,
        tcfExpectedStdout = if hasOut then Just outFile else Nothing
      }
