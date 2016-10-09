module Main where

import           Servant.JS

import           Server

main :: IO ()
main = writeJSForAPI api jquery "static/client.js"
