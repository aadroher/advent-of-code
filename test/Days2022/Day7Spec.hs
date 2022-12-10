{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2022.Day7Spec (spec) where

import Days2022.Day7
  ( Command (..),
    Dir (..),
    DirReference (..),
    File (..),
    FileTree (..),
    ParsedLine (..),
    addFileTree,
    parseFileTree,
    parseLine,
  )
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "exercise 1" $ do
    describe "parseLine" $ do
      it "$ cd / -> ParsedCommand (Cd Root)" $ do
        parseLine "$ cd /" `shouldBe` ParsedCommand (Cd Root)
      it "$ cd .. -> ParsedCommand (Cd Parent)" $ do
        parseLine "$ cd .." `shouldBe` ParsedCommand (Cd Parent)
      it "$ cd somedir -> ParsedCommand (Cd (Child 'somedir'))" $ do
        parseLine "$ cd somedir" `shouldBe` ParsedCommand (Cd (Child "somedir"))
      it "$ ls -> ParsedCommand Ls" $ do
        parseLine "$ ls" `shouldBe` ParsedCommand Ls
      it "dir somedir -> ParsedNode (Dir 'somedir')" $ do
        parseLine "dir somedir" `shouldBe` ParsedDir (Dir "somedir")
      it "62596 h.lst -> ParsedNode (File 62596 'h.lst')" $ do
        parseLine "62596 h.lst" `shouldBe` ParsedFile (File 62596 "h.lst")

    describe "addFileTree" $ do
      it "adds a file to the root" $ do
        let ft = Node (Dir "/") []
        let newFt = addFileTree (Dir "/") (Leaf (File 500 "a.txt")) ft
        let expectedFt = Node (Dir "/") [Leaf (File 500 "a.txt")]
        newFt `shouldBe` expectedFt
      it "adds a dir to the root" $ do
        let ft = Node (Dir "/") []
        let newFt = addFileTree (Dir "/") (Node (Dir "a") []) ft
        let expectedFt = Node (Dir "/") [Node (Dir "a") []]
        newFt `shouldBe` expectedFt
      it "adds a file to a descendent" $ do
        let ft = Node (Dir "/") [Node (Dir "a") [], Node (Dir "b") []]
        let newFt = addFileTree (Dir "b") (Leaf (File 500 "a.txt")) ft
        let expectedFt =
              Node
                (Dir "/")
                [ Node (Dir "a") [],
                  Node (Dir "b") [Leaf (File 500 "a.txt")]
                ]
        newFt `shouldBe` expectedFt

    describe "parseFileTree" $ do
      let terminalLines =
            [ "$ cd /",
              "$ ls",
              "dir a",
              "dir b",
              "10000 c.txt",
              "$ cd a",
              "$ ls",
              "12000 d.txt",
              "$ cd ..",
              "$ ls",
              "dir a",
              "dir b",
              "10000 c.txt",
              "$ cd b",
              "$ ls",
              "15000 e.txt"
            ]
      let parsedLines = parseLine <$> terminalLines
      it "parses minimal example" $ do
        let expectedFileTree =
              Node
                (Dir "/")
                [ Node
                    (Dir "a")
                    [ Leaf (File 12000 "d.txt")
                    ],
                  Node
                    (Dir "b")
                    [ Leaf (File 15000 "e.txt")
                    ],
                  Leaf (File 10000 "c.txt")
                ]
        let rootFt = Node (Dir "/") []
        parseFileTree parsedLines (Dir "/") rootFt `shouldBe` expectedFileTree
