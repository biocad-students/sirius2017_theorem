{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Generic
import System.Console.Haskeline



main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "> "
           case minput of
               Nothing -> return ()
               Just "off" -> return ()
               Just "exit" -> return ()
               Just input -> do outputStrLn $ input ++ "!"
                                loop

