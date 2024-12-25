# About

A basic Git-Like Version Control System implementation in Haskell. I made it mostly to learn haskell
but it has most of the basic functions:

1. No Staging are, directly do `commitTree` and it will create a commit on top of existing commit.
   If specified branch is different from base branch, `git checkout -b <targetBranch>` equivalent is implicit.
2. See history using `gitLog`
3. Checkout to any branch using: `checkoutBranch`.
4. Checkout to any commit using: `checkoutCommit`. Detached head mode is not supported however, i.e creating commit on top of this commit is UB :P

I've not added cli part, so testing can be done using interactive shell as explained in next section:

# Testing

Create `testRepo` directory, add some files and folders.

`testDir` will be where metadata will be stored. It's outside repo folder because for now this client wouldn't ignore it.

Then run:

```
$ cabal repl
ghci> let fsStore = FileSystemStore "testDir"
ghci> let gitClient = GitClient ("testRepo", fsStore)

ghci> commitTree gitClient "master" "Initial Commit"
prev commit for branch: <NULL COMMIT>
Reading head commit
prev commit: <NULL COMMIT>

-- Do some changes to test Repo

ghci> commitTree gitClient "master" "Commit - 2"
prev commit for branch: aeee54dbcb968288611f25df54fb13c5ba1a886f
prev commit: aeee54dbcb968288611f25df54fb13c5ba1a886f

-- Do some changes to test Repo

ghci> commitTree gitClient "side" "Side commit"
prev commit for branch: <NULL COMMIT>
Reading head commit
prev commit: 938d377667493a650df59c7665848255c2ece568

ghci> checkoutBranch gitClient "side"
Loading tree: 1bca87cd6db7a5f8865853d1aa38469fd863f4eb

ghci> checkoutBranch gitClient "master"
Loading tree: e01d4154a6c9eb6f9e21bd65dd3017a277ffdbe3

ghci> checkoutBranch gitClient "side"
Loading tree: 1bca87cd6db7a5f8865853d1aa38469fd863f4eb

ghci> gitLog gitClient "master"
-----------------------------------------
Commit: Commit - 2
TreeID: e01d4154a6c9eb6f9e21bd65dd3017a277ffdbe3
Commit ID: 938d377667493a650df59c7665848255c2ece568
-----------------------------------------
Commit: Initial Commit
TreeID: b0ecffd509bdf6f442dc26efadb278cf38ee8f6a
Commit ID: aeee54dbcb968288611f25df54fb13c5ba1a886f
-----------------------------------------
---END OF HISTORY---
-----------------------------------------
```
