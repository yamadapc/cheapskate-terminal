module Main where

import           Cheapskate
import           Cheapskate.Terminal
import qualified Data.Text.IO        as Text (readFile)
import           System.Environment  (getArgs)

main :: IO ()
main = do
    (f:_) <- getArgs
    prettyPrint =<< markdown def <$> Text.readFile f
