-- import Control.Monad             (liftM)
import Data.List                 (intersperse, intercalate)
import Test.QuickCheck.Gen       (elements, listOf, Gen, oneof, sample, sample')
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Property  (Property, property, collect, classify, forAll)
import Test.QuickCheck.Test      (quickCheck)
import Lib                       (splitFN_0, joinFN_0, splitFN_1)
import GenLib                    (identifier, opt)

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

fileNames :: Gen String
fileNames = do
  name <- opt identifier
  dot <- opt (return ".")
  ext <- opt identifier
  exts <- listOf identifier
  oneof [ return $ name ++ dot ++ ext -- this would cover both with and without dots
        , return $ name ++ "." ++ (intercalate "." $ exts)
        , return $ name ++ "." ++ (concat . intersperse "." $ exts)
        ]

prop_fileNames_are_roundTrippable_3 :: Property
prop_fileNames_are_roundTrippable_3 =
  forAll fileNames $ \fn ->
  joinFN_0 (splitFN_1 fn) == fn

noExtFilenames :: Gen String
noExtFilenames = do
  name <- identifier
  dot <- opt (return ".")
  return (dot ++ name)

prop_names_equal_fileNames_0 :: Property
prop_names_equal_fileNames_0 =
  forAll noExtFilenames $ \fn ->
  let (name, _) = splitFN_1 fn
  in name == fn

main :: IO ()
main = do
  -- quickCheck prop_fileNames_are_roundTrippable_0
  -- quickCheck prop_fileNames_are_roundTrippable_1
  -- quickCheck prop_fileNames_are_roundTrippable_2
  quickCheck prop_fileNames_are_roundTrippable_3
  quickCheck prop_names_equal_fileNames_0
