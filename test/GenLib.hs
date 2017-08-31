module GenLib (identifier, opt) where

import Test.QuickCheck.Gen (elements, listOf, Gen, oneof)

iden0 :: Gen Char
iden0 = oneof [elements ['a'..'z'], elements ['A'..'Z'], elements ['0'..'9']]

idenN :: Gen String
idenN = listOf iden0

opt :: Gen String -> Gen String
opt g = oneof [g, return ""]

identifier :: Gen String
identifier = iden0 >>= 
             \i0 -> idenN >>=
             return . (i0:)
