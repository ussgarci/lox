{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad.State (execState)
import qualified Data.Text as T
import qualified MegaScanner as MS
import qualified Scanner as S
import qualified StatefulScanner as SS
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import qualified Token as TK

main :: IO ()
main = defaultMain tests

hasErrors :: [Either a b] -> Bool
hasErrors = any isLeft
  where
    isLeft (Left _) = True
    isLeft (Right _) = False

-- Scanner
scannerUnitTests :: TestTree
scannerUnitTests =
    testGroup
        "Scanner unit tests"
        [ testCase "No errors scanning data/parens.lox" $ do
            content <- readFile "data/parens.lox"
            let errorsFound = hasErrors (S.scanTokens content)
            errorsFound @?= False
        , testCase "No errors scanning data/scanning.lox" $ do
            content <- readFile "data/scanning.lox"
            let errorsFound = hasErrors (S.scanTokens content)
            errorsFound @?= False
        , testCase "No errors scanning data/smarter.lox" $ do
            content <- readFile "data/smarter.lox"
            let errorsFound = hasErrors (S.scanTokens content)
            errorsFound @?= False
        ]

-- Unit tests group.
megaScannerUnitTests :: TestTree
megaScannerUnitTests =
    testGroup
        "MegaScanner unit tests"
        [ testCase "No errors scanning data/parens.lox" $ do
            content <- readFile "data/parens.lox"
            let results = MS.scan content :: Either (ParseErrorBundle String T.Text) [TK.Token]
            case results of
                Left bundle -> assertFailure (show bundle)
                Right _ -> assertBool "The result should have no errors" True
        , testCase "No errors scanning data/scanning.lox" $ do
            content <- readFile "data/scanning.lox"
            let results = MS.scan content :: Either (ParseErrorBundle String T.Text) [TK.Token]
            case results of
                Left bundle -> assertFailure (show bundle)
                Right _ -> assertBool "The result should have no errors" True
        , testCase "No errors scanning data/smarter.lox" $ do
            content <- readFile "data/smarter.lox"
            let results = MS.scan content :: Either (ParseErrorBundle String T.Text) [TK.Token]
            case results of
                Left bundle -> assertFailure (show bundle)
                Right _ -> assertBool "The result should have no errors" True
        ]

isAtEndUnitTests :: TestTree
isAtEndUnitTests =
    testGroup --
        "isAtEnd unit tests"
        [ testCase "Returns False when current < length" $ do
            let text = T.pack "some text"
            let state = SS.ScannerState text 0 0 1 [] []
            SS.isAtEnd state @?= False
        , testCase "Returns False when current is mid-text" $ do
            let text = T.pack "some text"
            let state = SS.ScannerState text 2 5 1 [] []
            SS.isAtEnd state @?= False
        , testCase "Returns True when current == length" $ do
            let text = T.pack "some text"
            let state = SS.ScannerState text (T.length text) (T.length text) 1 [] []
            SS.isAtEnd state @?= True
        , testCase "Returns True when current > length" $ do
            let text = T.pack "some text"
            let state = SS.ScannerState text (T.length text) (T.length text + 1) 1 [] []
            SS.isAtEnd state @?= True
        , testCase "Returns True for empty source text" $ do
            let state = SS.ScannerState T.empty 0 0 1 [] []
            SS.isAtEnd state @?= True
        ]

symbols :: [String]
symbols =
    [ "("
    , ")"
    , "{"
    , "}"
    , ","
    , "."
    , "-"
    , "+"
    , ";"
    , "*"
    , "="
    , "!"
    , "<"
    , ">"
    , "/"
    ]

scanTokenUnitTests :: TestTree
scanTokenUnitTests =
    testGroup --
        "scanToken unit tests"
        [ testCase "Scans symbols" $ do
            let text = T.pack $ concat symbols
            let state = SS.ScannerState text 0 0 1 [] []
            let result = execState SS.scanTokens state
            -- mapM_ (putStrLn . show) $ result.tokens
            length result.tokens @?= length symbols
            result.current @?= length symbols
            result.start @?= length symbols
            length result.errors @?= 0
        ]
tests :: TestTree
tests = testGroup "Tests" [isAtEndUnitTests, scanTokenUnitTests]
