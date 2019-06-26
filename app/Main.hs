module Main where

import Text.Pandoc.Builder (toList)
import Text.Pandoc.JSON (toJSONFilter)
import Env (convertBlock)

main :: IO ()
main = toJSONFilter $ toList . convertBlock
