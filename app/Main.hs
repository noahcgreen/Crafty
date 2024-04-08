module Main where

import qualified Crafty.Parse 

main :: IO ()
-- main = do
--   let file = "test.scm"
--   source <- readFile file
--   case Crafty.Parse.tokenize "input" source of
--     Left error' -> print error'
--     Right tokens -> mapM_ print tokens

main = do
    let file = "test.scm"
    let source = "10"
    case Crafty.Parse.read file source of
        Left error' -> print error'
        Right datum -> print datum
