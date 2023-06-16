module Main where

import qualified MyMod as MM

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    let x = True
    putStrLn (MM.myFn x)
