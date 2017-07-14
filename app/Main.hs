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
<<<<<<< HEAD
eval input = case parseTerm input of
  Right term -> case runExcept (typeOf term) of
    Right term -> show term
    Left err -> show err
  Left err -> err
=======
eval input = case parseTermM input of
    Just term -> show term
    Nothing -> show "!" 
>>>>>>> f8e6ed779055fdaf691fc6124c8e35d6d0cd3aa8
