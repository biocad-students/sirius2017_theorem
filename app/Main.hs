{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Generic
import System.Console.Haskeline
import Lawer


main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "> "
           case minput of
               Nothing -> return ()
               Just ":q" -> return ()
               Just ":quit" -> return ()
               Just input -> do outputStrLn $ eval input
                                loop


eval :: String -> String
eval input = case parseTermM input of
	Just term -> show term
	Nothing -> show "!" 