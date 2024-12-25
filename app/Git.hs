module Git where

import ContentStore
import Control.Monad (join)
import Crypto.Hash (Digest, SHA1, hash)
import Data.ByteString.Char8 (pack)

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

commitTree :: GitClient -> String -> IO GitClient
commitTree client commitMsg = do
  (treeID, nextClient) <- writeTree client
  let commitObj = CommitObject (treeID, commitMsg)

  return nextClient

writeTree :: GitClient -> IO (ObjectID, GitClient)
writeTree = undefined

data ObjectType = TypeBlob | TypeTree | TypeCommit deriving (Eq, Show)

type ObjectID = String -- sha 1 hash

data RawObject = ObjectData
  { objType :: ObjectType,
    objLength :: Int,
    objData :: String
  }
  deriving (Show)

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
  let (header, content) = span (/= '\0') rawData
      (objTypeStr, objLengthStr) = break (== ' ') header
      objectType = case objTypeStr of
        "blob" -> TypeBlob
        "tree" -> TypeTree
        "commit" -> TypeCommit
        _ -> error $ "Unknown object type: " ++ objTypeStr
      objectLength = read (drop 1 objLengthStr) :: Int
   in ObjectData
        { objType = objectType,
          objLength = objectLength,
          objData = drop 1 content
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
  join -- YAY I THOUGHT OF MONAD JOIN
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
  let finalStr = join [serializeType $ objType rawObj, show $ objLength rawObj, objData rawObj]
      id = sha1Hash finalStr
   in (finalStr, id)
