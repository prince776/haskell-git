module Main where

import ContentStore

main :: IO ()
main = do
    let contentStore = FileSystemStore "testDir"
    store contentStore "test-key" "test-value"
    testVal <- fetch contentStore "test-key"
    case testVal of
        Just x -> putStrLn x
        otherwise-> putStrLn "WO"
    return ()
