module ContentStore where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

class ContentStore s where
  store :: s -> String -> String -> IO s -- Takes the store, a key, and a value, returns an updated store
  fetch :: s -> String -> IO (Maybe String) -- Takes the store and a key, returns a value (or Nothing if not found)

newtype FSContentStore = FileSystemStore FilePath

instance ContentStore FSContentStore where
    store (FileSystemStore baseDir) key value = do
        let (dir, file) = splitKey key
        let dirPath = baseDir </> dir
        let filePath = dirPath </> file
        createDirectoryIfMissing True dirPath
        writeFile filePath value
        return (FileSystemStore baseDir)

    fetch (FileSystemStore baseDir) key = do
      let (dir, file) = splitKey key
      let filePath = baseDir </> dir </> file
      exists <- doesFileExist filePath        -- Check if the file exists
      if exists
        then fmap Just $ readFile filePath
        else return Nothing     

splitKey :: String -> (String, String)
splitKey key = (take 2 key, drop 2 key)
