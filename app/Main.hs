module Main where

import Text.Pandoc.JSON (toJSONFilter)
import Env (convertBlock)

main :: IO ()
main = toJSONFilter convertBlock
