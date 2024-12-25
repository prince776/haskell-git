module ContentStore where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

class ContentStore s where
  store :: s -> String -> String -> IO () -- Takes the store, a key, and a value, returns an updated store
  fetch :: s -> String -> IO (Maybe String) -- Takes the store and a key, returns a value (or Nothing if not found)

newtype FSContentStore = FileSystemStore FilePath

instance ContentStore FSContentStore where
  store (FileSystemStore baseDir) key value = do
    let (dir, file) = splitKey key
    let dirPath = baseDir </> dir
    let filePath = dirPath </> file
    createDirectoryIfMissing True dirPath
    writeFile filePath value
    return ()

  fetch (FileSystemStore baseDir) key = do
    let (dir, file) = splitKey key
    let filePath = baseDir </> dir </> file
    exists <- doesFileExist filePath
    if exists
      then fmap Just $ readFile filePath
      else return Nothing

storeIfNotExist :: (ContentStore s) => s -> String -> String -> IO ()
storeIfNotExist cs key value = do
  existing <- fetch cs key
  case existing of
    Just _ -> return ()
    Nothing -> store cs key value

splitKey :: String -> (String, String)
splitKey key = (take 2 key, drop 2 key)
