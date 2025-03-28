import Test.QuickCheck
--import Test.HUnit
import qualified Scanner as S

prop_scannerNoErrors :: FilePath -> Property
prop_scannerNoErrors filePath = ioProperty $ do
  fileContents <- readFile filePath
  let results = S.scanTokens fileContents
  let hasError = any isLeft results
  if hasError
    then do
      putStrLn "Scanner Errors found."
      return False
    else do
      putStrLn "Scanner processed file without errors."
      return True
  where
    isLeft (Left _) = True
    isLeft (Right _) = False

main :: IO ()
main = do 
  quickCheckWith (stdArgs { maxSize = 5, maxSuccess = 5 }) (prop_scannerNoErrors "data/parens.lox") 
