module Main where

import Network.Wai.Handler.Warp
import Server

main :: IO ()
main = do
  run 3030 app
