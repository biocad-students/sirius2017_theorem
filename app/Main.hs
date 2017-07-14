{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Generic
import System.Console.Haskeline
import Lawer
import Control.Monad.Trans.Except          (runExcept)

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
eval input = case parseTerm input of
  Right term -> case runExcept (typeOf term) of
    Right term -> show term
    Left err -> show err
  Left err -> err