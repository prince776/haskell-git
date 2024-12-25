module Git where

import ContentStore
import Control.Monad (filterM, join, (>=>))
import Crypto.Hash (Digest, SHA1, hash)
import Data.ByteString.Char8 (pack)
import Data.List (partition)
import qualified GHC.Base as Monad
import GHC.Read (readField)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeFileName, (</>))

{-
We are not gonna have a staging area, and granularity for commiting for simplicity.
Workflow would look like
git commit -m "Commit Name"
-- This will write-tree, and then create a commit.
-}

-- We can technically depend on the content store interface here, but it's fine, I don't wanna
-- complicate. Having an interface is still good as newer impl (if i ever make), will just need
-- to adhere to the interface, and switch here.
newtype GitClient = GitClient (String, FSContentStore) -- (RepoRoot, ContentStore)

commitTree :: GitClient -> String -> String -> IO ()
commitTree (GitClient (repoRoot, fsClient)) branch commitMsg = do
  treeEntry <- writeTree fsClient repoRoot
  let FileSystemStore fsRoot = fsClient
  prevCommitIDForBranch <- readBranchRef fsRoot branch
  putStrLn $ "prev commit for branch: " <> prevCommitIDForBranch
  prevCommitID <-
    if prevCommitIDForBranch == nullCommitID
      then do
        putStrLn "Reading head commit"
        readHEADCommitID fsRoot
      else do
        return prevCommitIDForBranch
  putStrLn $ "prev commit: " <> prevCommitID
  let commitObj = GitCommmit $ CommitObject (entryID treeEntry, prevCommitID, commitMsg)
  let (commitObjStr, commitObjStoreID) = serializeObj $ toRawObject commitObj
  storeIfNotExist fsClient commitObjStoreID commitObjStr
  updateBranchRef fsRoot branch commitObjStoreID
  updateHEADBranch fsRoot branch
  return ()

gitLog :: GitClient -> String -> IO ()
gitLog (GitClient (repoRoot, fsClient)) branch = do
  let FileSystemStore fsRoot = fsClient
  latestCommitID <- readBranchRef fsRoot branch
  logCommitFrom (GitClient (repoRoot, fsClient)) latestCommitID
  return ()

logCommitFrom :: GitClient -> ObjectID -> IO ()
logCommitFrom (GitClient (repoRoot, fsClient)) commitID = do
  if commitID == nullCommitID
    then do
      putStrLn "-----------------------------------------"
      putStrLn "---END OF HISTORY---"
      putStrLn "-----------------------------------------"
    else do
      commitSerMaybe <- fetch fsClient commitID
      let commitSer = case commitSerMaybe of
            Nothing -> undefined
            (Just x) -> x
      let rawObj = parseRawObject commitSer
      let gitObj = fromRawObject rawObj
      case gitObj of
        (GitCommmit (CommitObject (treeID, prevCommitID, msg))) -> do
          putStrLn "-----------------------------------------"
          putStrLn $ "Commit: " <> msg
          putStrLn $ "TreeID: " <> treeID
          putStrLn $ "Commit ID: " <> commitID
          logCommitFrom (GitClient (repoRoot, fsClient)) prevCommitID
        otherwise -> undefined

writeTree :: FSContentStore -> String -> IO TreeObjectEntry
writeTree fsClient dir = do
  isDir <- doesDirectoryExist dir
  gitObj <-
    if not isDir
      then do
        fileContents <- readFile dir
        let blobObj = GitBlob $ BlobObject fileContents
        return blobObj
      else do
        dirItems <- listDirectory dir

        subdirs <- filterM doesDirectoryExist dirItems
        files <- filterM (doesDirectoryExist >=> (pure . not)) dirItems

        blobEntries <- mapM (writeTree fsClient . (dir </>)) files
        subDirEntries <- mapM (writeTree fsClient . (dir </>)) subdirs

        let treeObj = GitTree $ TreeObject $ blobEntries ++ subDirEntries
        return treeObj

  let rawObj = toRawObject gitObj
  let (objSer, objSha) = serializeObj rawObj
  storeIfNotExist fsClient objSha objSer
  return TreeObjectEntry {entryMode = defaultEntryMode, entryID = objSha, entryType = objType rawObj, entryName = takeFileName dir}

data ObjectType = TypeBlob | TypeTree | TypeCommit deriving (Eq, Show)

type ObjectID = String -- sha 1 hash

data RawObject = ObjectData
  { objType :: ObjectType,
    objLength :: Int,
    objData :: String
  }
  deriving (Show)

defaultEntryMode :: String
defaultEntryMode = "00000"

data TreeObjectEntry = TreeObjectEntry
  { entryMode :: String,
    entryType :: ObjectType,
    entryID :: ObjectID,
    entryName :: String
  }
  deriving (Show)

newtype BlobObject = BlobObject String deriving (Show)

newtype TreeObject = TreeObject [TreeObjectEntry] deriving (Show)

{-
Data Format:
<treeObjectID>
<prevCommitID>
<Commit Message>
-}
newtype CommitObject = CommitObject (ObjectID, ObjectID, String) deriving (Show)

data GitObject = GitBlob BlobObject | GitTree TreeObject | GitCommmit CommitObject deriving (Show)

nullCommitID :: ObjectID
nullCommitID = "<NULL COMMIT>"

fromRawObject :: RawObject -> GitObject
fromRawObject rawObj
  | objType rawObj == TypeBlob = GitBlob $ BlobObject $ objData rawObj
  | objType rawObj == TypeTree = GitTree $ treeObjectFromRawData $ objData rawObj
  | objType rawObj == TypeCommit =
      let (treeID, rest) = break (== '\n') (objData rawObj)
          (prevCommitID, restMsg) = break (== '\n') (drop 1 rest)
          commitMsg = drop 1 restMsg
       in GitCommmit $ CommitObject (treeID, prevCommitID, commitMsg)
  | otherwise = undefined

toRawObject :: GitObject -> RawObject
toRawObject gitObj = case gitObj of
  GitBlob (BlobObject blobData) -> ObjectData {objType = TypeBlob, objLength = length blobData, objData = blobData}
  GitTree (TreeObject treeEntries) ->
    let rawData = join $ map (\e -> treeEntryToRaw e ++ "\n") treeEntries -- Yay mom, thought of fmap here, and join again
     in ObjectData {objType = TypeTree, objLength = length rawData, objData = rawData}
  GitCommmit (CommitObject (treeID, prevCommitID, commitMsg)) ->
    let rawData = treeID ++ "\n" ++ prevCommitID ++ "\n" ++ commitMsg
     in ObjectData {objType = TypeCommit, objLength = length rawData, objData = rawData}

parseRawObject :: String -> RawObject
parseRawObject rawData =
  let (objTypeStr, restFile) = span (/= ' ') rawData
      (objLengthStr, contentStr) = break (== ' ') $ drop 1 restFile
      objectType = case objTypeStr of
        "blob" -> TypeBlob
        "tree" -> TypeTree
        "commit" -> TypeCommit
        _ -> error $ "Unknown object type: " ++ objTypeStr
      objectLength = read (drop 1 objLengthStr) :: Int
      content = drop 1 contentStr
   in ObjectData
        { objType = objectType,
          objLength = objectLength,
          objData = content
        }

treeObjectFromRawData :: String -> TreeObject
treeObjectFromRawData rawData =
  let rawEntries = lines rawData
   in TreeObject $ map treeEntryFromRawLine rawEntries

treeEntryFromRawLine :: String -> TreeObjectEntry
treeEntryFromRawLine rawLine =
  let ws = words rawLine
   in TreeObjectEntry
        { entryMode = head ws,
          entryType = if ws !! 1 == "blob" then TypeBlob else TypeTree, -- Yeah whatever.
          entryID = ws !! 2,
          entryName = ws !! 3
        }

treeEntryToRaw :: TreeObjectEntry -> String
treeEntryToRaw entry =
  unwords -- YAY I THOUGHT OF MONAD JOIN
    [ entryMode entry,
      if entryType entry == TypeBlob then "blob" else "tree",
      entryID entry,
      entryName entry
    ]

-- Function to compute SHA1 hash
sha1Hash :: String -> String
sha1Hash input =
  let digest = hash (pack input) :: Digest SHA1
   in show digest

serializeType :: ObjectType -> String
serializeType objType = case objType of
  TypeBlob -> "blob"
  TypeTree -> "tree"
  TypeCommit -> "commit"

serializeObj :: RawObject -> (String, ObjectID) -- Data, ID
serializeObj rawObj =
  let finalStr = unwords [serializeType $ objType rawObj, show $ objLength rawObj, objData rawObj]
      id = sha1Hash finalStr
   in (finalStr, id)

headTracker :: String
headTracker = "HEAD"

refsSubDir :: FilePath -> FilePath
refsSubDir rootPath = rootPath </> "refs"

readHEADCommitID :: FilePath -> IO ObjectID
readHEADCommitID rootPath = do
  let headFile = rootPath </> headTracker
  headExists <- doesFileExist headFile
  if headExists
    then do
      branchName <- readFile headFile
      readBranchRef rootPath branchName
    else return nullCommitID

updateHEADBranch :: String -> String -> IO ()
updateHEADBranch rootPath branchName = do
  let headFile = rootPath </> headTracker
  writeFile headFile branchName

updateBranchRef :: FilePath -> String -> ObjectID -> IO ()
updateBranchRef rootPath branchName commitID = do
  createDirectoryIfMissing True $ refsSubDir rootPath
  writeFile (refsSubDir rootPath </> branchName) commitID

readBranchRef :: FilePath -> String -> IO ObjectID
readBranchRef rootDir branchName = do
  let fileName = refsSubDir rootDir </> branchName
  isRefPresent <- doesFileExist fileName
  if isRefPresent
    then
      readFile fileName
    else return nullCommitID
