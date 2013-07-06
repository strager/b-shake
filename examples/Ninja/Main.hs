{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import All (runNinja)
import System.Environment (getArgs)
import B.Shake

main :: IO ()
main = shake shakeOptions =<< runNinja "build.ninja" =<< getArgs
