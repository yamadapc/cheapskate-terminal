module Main where

import           Cheapskate
import           Cheapskate.Terminal
import qualified Data.Text.IO        as Text (readFile, getContents)
import           System.Environment  (getArgs)
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout (BlockBuffering (Just 500))
    args <- getArgs
    input <- case args of
        (f:_) -> Text.readFile f
        _-> Text.getContents
    prettyPrint (markdown def input)
