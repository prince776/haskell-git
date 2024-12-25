# Testing

```
$ cabal repl
ghci> let fsStore = FileSystemStore "testDir"
ghci> let gitClient = GitClient ("testRepo", fsStore)
ghci> commitTree gitClient "Test commit"
ghci> gitLog gitClient "master"
```
