module Lib where

splitFN_0 :: String -> (String, String)
splitFN_0 fileName = 
  let (p1, p2) = span (/= '.') . reverse $ fileName 
  in
    case (length p2 == 0) of 
      True -> (reverse p1, "") -- no suffix
      False -> (prefix, suffix)
        where
          prefix = reverse $ drop 1 p2 -- removing the first char that is a dot
          suffix = '.' : reverse p1

joinFN_0 :: (String, String) -> String
joinFN_0 (name, ext) = name ++ ext
