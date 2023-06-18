{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified MyMod as MM
import qualified Lexer as L
import qualified Parser as P

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    let x = True
    putStrLn (MM.myFn x)
    -- print (L.scanMany "ab xx x dsf' identtt  ")
    -- print (L.scanMany "ab \\x.x x dsf' identtt  )(")
    --print (L.scanMany "ab \\x.x x /* dsf'*/ /*/**/*/ ide.--nttt  )(")
    print (L.runAlex "\\x.x x" P.parseUntyped)
    print (L.runAlex "\\x.x (\\yz . y (yz x))/**/ (x)" P.parseUntyped)
