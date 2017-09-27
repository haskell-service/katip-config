module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-isrc", "src/Katip/Config.hs"]
