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
    getDirs,
    getFileSizeSum,
    getFileTreesOfSizeLE,
    getSizeSumOfFileTreesOfSizeLE,
    getSubtree,
    parseFileTreeFromRoot,
    parseLine,
  )
import Import
import qualified RIO.List as L
import qualified RIO.Set as S
import qualified RIO.Text as T
import Test.Hspec
import Text.Pretty.Simple (pPrint)
import Util (getFilePath)

exampleTerminalLines :: [Text]
exampleTerminalLines =
  [ "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k"
  ]

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
        let ft = Node (Dir "/") S.empty
        let newFt = addFileTree (Dir "/") (Leaf (File 500 "a.txt")) ft
        let expectedFt = Node (Dir "/") $ S.fromList [Leaf (File 500 "a.txt")]
        newFt `shouldBe` expectedFt
      it "adds a dir to the root" $ do
        let ft = Node (Dir "/") S.empty
        let newFt = addFileTree (Dir "/") (Node (Dir "a") S.empty) ft
        let expectedFt = Node (Dir "/") $ S.fromList [Node (Dir "a") S.empty]
        newFt `shouldBe` expectedFt
      it "adds a file to a descendent" $ do
        let ft = Node (Dir "/") $ S.fromList [Node (Dir "a") S.empty, Node (Dir "b") S.empty]
        let newFt = addFileTree (Dir "b") (Leaf (File 500 "a.txt")) ft
        let expectedFt =
              Node
                (Dir "/")
                $ S.fromList
                  [ Node (Dir "a") S.empty,
                    Node (Dir "b") $ S.fromList [Leaf (File 500 "a.txt")]
                  ]
        newFt `shouldBe` expectedFt

    describe "getFileSizeSum" $ do
      it "works on example" $ do
        let fileTree =
              Node
                (Dir "/")
                $ S.fromList
                  [ Node
                      (Dir "a")
                      $ S.fromList
                        [ Leaf (File 12000 "d.txt")
                        ],
                    Node
                      (Dir "b")
                      $ S.fromList
                        [ Leaf (File 15000 "e.txt")
                        ],
                    Leaf (File 10000 "c.txt")
                  ]
        getFileSizeSum fileTree `shouldBe` 37000

    describe "getSubtree" $ do
      it "gets subtree with b root" $ do
        let fileTree =
              Node
                (Dir "/")
                $ S.fromList
                  [ Node
                      (Dir "a")
                      $ S.fromList
                        [ Leaf (File 12000 "d.txt")
                        ],
                    Node
                      (Dir "b")
                      $ S.fromList
                        [ Leaf (File 15000 "e.txt")
                        ],
                    Leaf (File 10000 "c.txt")
                  ]
        let expectedFileTree =
              Node
                (Dir "b")
                $ S.fromList
                  [ Leaf (File 15000 "e.txt")
                  ]
        getSubtree (Dir "b") fileTree `shouldBe` Just expectedFileTree

    describe "getDirs" $ do
      it "gets the dirs from the example" $ do
        let fileTree =
              Node
                (Dir "/")
                $ S.fromList
                  [ Node
                      (Dir "a")
                      $ S.fromList
                        [ Leaf (File 12000 "d.txt")
                        ],
                    Node
                      (Dir "b")
                      $ S.fromList
                        [ Leaf (File 15000 "e.txt")
                        ],
                    Leaf (File 10000 "c.txt")
                  ]
        getDirs fileTree
          `shouldBe` [ Dir "/",
                       Dir "a",
                       Dir "b"
                     ]

    xdescribe "getFileTreesOfSizeLE" $ do
      let fileTree =
            Node
              (Dir "/")
              $ S.fromList
                [ Node
                    (Dir "a")
                    $ S.fromList
                      [ Leaf (File 12000 "d.txt")
                      ],
                  Node
                    (Dir "b")
                    $ S.fromList
                      [ Leaf (File 15000 "e.txt")
                      ],
                  Leaf (File 10000 "c.txt")
                ]
      it "12000 -> [a]" $ do
        getFileTreesOfSizeLE 12000 fileTree `shouldBe` [(Dir "a", 12000)]
      it "15000 -> [a, b]" $ do
        getFileTreesOfSizeLE 15000 fileTree
          `shouldBe` [ (Dir "a", 12000),
                       (Dir "b", 15000)
                     ]
      it "40000 -> [/, a, b]" $ do
        getFileTreesOfSizeLE 40000 fileTree
          `shouldBe` [ (Dir "/", 37000),
                       (Dir "a", 12000),
                       (Dir "b", 15000)
                     ]
      it "solves the example" $ do
        let parsedLines = parseLine <$> exampleTerminalLines
        let fileTree = parseFileTreeFromRoot parsedLines
        getFileTreesOfSizeLE 100000 fileTree `shouldBe` [(Dir "a", 94853), (Dir "e", 584)]

    xdescribe "getSizeSumOfFileTreesOfSizeLE" $ do
      it "solves the example" $ do
        let parsedLines = parseLine <$> exampleTerminalLines
        let fileTree = parseFileTreeFromRoot parsedLines
        getSizeSumOfFileTreesOfSizeLE 100000 fileTree `shouldBe` 95437

      it "solves the received data" $ do
        let filePath = getFilePath "22-7-1"
        -- pPrint filePath
        fileContents <- readFileUtf8 filePath
        let inputValues = parseLine <$> T.lines fileContents
        let fileTree = parseFileTreeFromRoot $ L.drop 1 inputValues
        -- pPrint fileTree
        getSizeSumOfFileTreesOfSizeLE 100000 fileTree `shouldBe` 7726269

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
      let parsedLines = parseLine <$> L.drop 1 terminalLines

      it "parses minimal example" $ do
        let expectedFileTree =
              Node
                (Dir "/")
                $ S.fromList
                  [ Node
                      (Dir "a")
                      $ S.fromList
                        [ Leaf (File 12000 "d.txt")
                        ],
                    Node
                      (Dir "b")
                      $ S.fromList
                        [ Leaf (File 15000 "e.txt")
                        ],
                    Leaf (File 10000 "c.txt")
                  ]
        parseFileTreeFromRoot parsedLines `shouldBe` expectedFileTree
