{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad
import Control.Monad (when)
import Data.Char (toUpper)
import System.Console.GetOpt
import System.Environment
import System.Environment (getArgs)
import System.Exit
import System.IO

import System.Console.ArgParser 

import Lib ( interpretAndFilterEventPlayScript
           , InputFile
           , Filter
           , OutputText )

data Arguments = Arguments String String deriving (Show)

argumentParser :: ParserSpec Arguments

argumentParser = Arguments
    `parsedBy` reqPos "file" `Descr` "File containing event play script"
    `andBy` optFlag "" "filter" `Descr` "Which perspective to filter"

applyArgsToInterpreter :: (InputFile -> Filter -> OutputText) -> Arguments -> IO ()

applyArgsToInterpreter interpretAndFilterEventPlayScript arguments = putStrLn "hello from applyArgsToInterpreter"

main :: IO ()
main = withParseResult argumentParser (applyArgsToInterpreter interpretAndFilterEventPlayScript)
