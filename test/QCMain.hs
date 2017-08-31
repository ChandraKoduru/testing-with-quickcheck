-- import Control.Monad             (liftM)
-- import Data.List                 (intersperse)
import Test.QuickCheck.Gen (elements, listOf)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Property  (Property, property, collect, classify)
import Test.QuickCheck.Test      (quickCheck)
import Lib                       (splitFN_0, joinFN_0)

newtype FileName = FileName {fileName :: String} deriving Show

instance Arbitrary FileName where
  arbitrary = 
    do 
      name <- elements ["foo", "bar", "baz"]
      ext <- listOf $ elements ['a'..'z']
      ext' <- elements ["." ++ ext, ""]
      return $ FileName (name ++ ext')

prop_fileNames_are_roundTrippable_0 :: FileName -> Property
prop_fileNames_are_roundTrippable_0 fileNameInst =
  property $ joinFN_0 (splitFN_0 fileName') == fileName'
  where
    fileName' = fileName fileNameInst

prop_fileNames_are_roundTrippable_1 :: FileName -> Property
prop_fileNames_are_roundTrippable_1 fileNameInst =
  collect fileName' $ 
  property $ 
  joinFN_0 (splitFN_0 fileName') == fileName'
  where
    fileName' = fileName fileNameInst

prop_fileNames_are_roundTrippable_2 :: FileName -> Property
prop_fileNames_are_roundTrippable_2 fileNameInst =
  classify (length ext == 0) "no ext" $
  classify (length ext > 0 && length ext < 5) "normal ext" $
  classify (length ext >= 5) "long ext" $
  property $ 
  joinFN_0 (splitFN_0 fileName') == fileName'
  where
    fileName' = fileName fileNameInst
    (_, ext) = splitFN_0 fileName'


main :: IO ()
main = do
  -- quickCheck prop_fileNames_are_roundTrippable_0
  -- quickCheck prop_fileNames_are_roundTrippable_1
  quickCheck prop_fileNames_are_roundTrippable_2
