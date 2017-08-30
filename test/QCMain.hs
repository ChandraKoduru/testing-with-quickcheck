-- import Control.Monad             (liftM)
-- import Data.List                 (intersperse)
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Property  (Property, property)
import Test.QuickCheck.Test      (quickCheck)
import Lib                       (splitFN_0, joinFN_0)

newtype FileName = FileName {fileName :: String} deriving Show

instance Arbitrary FileName where
  arbitrary = 
    do 
      name <- elements ["foo", "bar", "baz"]
      ext <- listOf $ elements ['a'..'z']
      return $ FileName (name ++ "." ++ ext)

prop_fileNames_are_roundTrippable_0 :: FileName -> Property
prop_fileNames_are_roundTrippable_0 fileNameInst =
  property $ joinFN_0 (splitFN_0 fileName') == fileName'
  where
    fileName' = fileName fileNameInst


main :: IO ()
main = quickCheck prop_fileNames_are_roundTrippable_0
