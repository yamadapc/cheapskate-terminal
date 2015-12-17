module Main where

import           Cheapskate
import           Cheapskate.Terminal
import qualified Data.Text.IO        as Text (readFile)
import           System.Environment  (getArgs)
import           System.IO

main :: IO ()
main = do
    (f:_) <- getArgs
    hSetBuffering stdout (BlockBuffering (Just 500))
    prettyPrint =<< markdown def <$> Text.readFile f
