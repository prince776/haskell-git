module Git where

import ContentStore
import Control.Monad (filterM, join, (>=>))
import Crypto.Hash (Digest, SHA1, hash)
import Data.ByteString.Char8 (pack)
import Data.List (partition)
import qualified GHC.Base as Monad
import System.Directory (doesDirectoryExist, listDirectory)
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

commitTree :: GitClient -> String -> IO ()
commitTree (GitClient (repoRoot, fsClient)) commitMsg = do
  treeEntry <- writeTree fsClient repoRoot
  let commitObj = GitCommmit $ CommitObject (entryID treeEntry, commitMsg)
  let (commitObjStr, commitObjStoreID) = serializeObj $ toRawObject commitObj
  storeIfNotExist fsClient commitObjStoreID commitObjStr
  return ()

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
<Commit Message>
-}
newtype CommitObject = CommitObject (ObjectID, String) deriving (Show)

data GitObject = GitBlob BlobObject | GitTree TreeObject | GitCommmit CommitObject deriving (Show)

fromRawObject :: RawObject -> GitObject
fromRawObject rawObj
  | objType rawObj == TypeBlob = GitBlob $ BlobObject $ objData rawObj
  | objType rawObj == TypeTree = GitTree $ treeObjectFromRawData $ objData rawObj
  | objType rawObj == TypeCommit =
      let (treeID, rest) = break (== '\n') (objData rawObj)
          commitMsg = drop 1 rest
       in GitCommmit $ CommitObject (treeID, commitMsg)
  | otherwise = undefined

toRawObject :: GitObject -> RawObject
toRawObject gitObj = case gitObj of
  GitBlob (BlobObject blobData) -> ObjectData {objType = TypeBlob, objLength = length blobData, objData = blobData}
  GitTree (TreeObject treeEntries) ->
    let rawData = join $ map (\e -> treeEntryToRaw e ++ "\n") treeEntries -- Yay mom, thought of fmap here, and join again
     in ObjectData {objType = TypeTree, objLength = length rawData, objData = rawData}
  GitCommmit (CommitObject (treeID, commitMsg)) ->
    let rawData = treeID ++ "\n" ++ commitMsg
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
