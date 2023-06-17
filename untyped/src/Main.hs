{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified MyMod as MM
import qualified Lexer as L

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    let x = True
    putStrLn (MM.myFn x)
    -- print (L.scanMany "ab xx x dsf' identtt  ")
    -- print (L.scanMany "ab \\x.x x dsf' identtt  )(")
    print (L.scanMany "ab \\x.x x /* dsf'*/ /*/**/*/ ide.--nttt  )(")
