module Interpreter.Show where

import System.Console.ANSI

surroundSGR :: [SGR] -> String -> String
surroundSGR sgrs str = setSGRCode sgrs ++ str ++ setSGRCode [Reset]