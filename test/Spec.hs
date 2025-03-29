--import Test.QuickCheck
--import Test.HUnit
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import qualified Data.Text as T
import qualified Scanner as S
import qualified MegaScanner as MS

main :: IO()
main = defaultMain tests

hasErrors :: [Either a b] -> Bool
hasErrors = any isLeft
  where
    isLeft (Left _) = True
    isLeft (Right _) = False

-- Scanner
scannerUnitTests :: TestTree
scannerUnitTests = testGroup "Scanner unit tests"
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
megaScannerUnitTests = testGroup "MegaScanner unit tests"
  [ testCase "No errors scanning data/parens.lox" $ do
      content <- readFile "data/parens.lox"
      let results = MS.scan content :: Either (ParseErrorBundle String T.Text) [MS.MegaToken]
      case results of
        Left bundle -> assertFailure (show bundle)
        Right _ -> assertBool "The result should have no errors" True
  , testCase "No errors scanning data/scanning.lox" $ do
      content <- readFile "data/scanning.lox"
      let results = MS.scan content :: Either (ParseErrorBundle String T.Text) [MS.MegaToken]
      case results of
        Left bundle -> assertFailure (show bundle)
        Right _ -> assertBool "The result should have no errors" True
  , testCase "No errors scanning data/smarter.lox" $ do
      content <- readFile "data/smarter.lox"
      let results = MS.scan content :: Either (ParseErrorBundle String T.Text) [MS.MegaToken]
      case results of
        Left bundle -> assertFailure (show bundle)
        Right _ -> assertBool "The result should have no errors" True
  ]

tests :: TestTree
tests = testGroup "Tests" [scannerUnitTests, megaScannerUnitTests]



