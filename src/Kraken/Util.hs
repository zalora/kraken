module Kraken.Util where

import Data.Char

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
