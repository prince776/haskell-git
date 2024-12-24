module Git where

data ObjectType = TypeBlob | TypeTree deriving (Eq, Show)

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

data GitObject = GitBlob BlobObject | GitTree TreeObject deriving (Show)

fromRawObject :: RawObject -> GitObject
fromRawObject rawObj
  | objType rawObj == TypeBlob = GitBlob $ BlobObject $ objData rawObj
  | objType rawObj == TypeTree = GitTree $ treeObjectFromRawData $ objData rawObj
  | otherwise = undefined

parseRawObject :: String -> RawObject
parseRawObject rawData =
  let (header, content) = span (/= '\0') rawData
      (objTypeStr, objLengthStr) = break (== ' ') header
      objectType = case objTypeStr of
        "blob" -> TypeBlob
        "tree" -> TypeTree
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
