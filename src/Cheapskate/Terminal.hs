{-# LANGUAGE OverloadedStrings #-}
module Cheapskate.Terminal
  where

import           Cheapskate
import           Control.Monad                (forM_)
import           Data.Monoid
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import           System.Console.ANSI
import           System.Console.Terminal.Size

testDocument = markdown def $ Text.unlines
    [ "# Hello World"
    , ""
    , "Look at me today"
    ]

prettyPrint :: Doc -> IO ()
prettyPrint (Doc _ blocks) = do
    putStrLn ""
    width <- size >>= \s -> case s of
        Just (Window _ w) -> return w
        Nothing -> return 80
    forM_ blocks $ \block -> do
        prettyPrintBlock width block
        putStrLn ""

prettyPrintBlock :: Int -> Block -> IO ()
prettyPrintBlock width (Header level els) = do
    setSGR [ SetColor Foreground Vivid Black
           , SetConsoleIntensity BoldIntensity
           ]
    putStr (take level (repeat '#'))
    putStr " "
    setSGR [ Reset ]
    setSGR [ SetColor Foreground Vivid Cyan
           , SetConsoleIntensity BoldIntensity
           ]
    mapM_ prettyPrintInline els
    setSGR [ Reset ]
    putStrLn ""
prettyPrintBlock width (Para els) = do
    mapM_ prettyPrintInline els
    putStrLn ""
prettyPrintBlock width (List _ (Bullet c) bss) = do
    forM_ bss $ \bs -> do
        setSGR [ SetColor Foreground Vivid Black ]
        putStr ("  " ++ (c:" "))
        setSGR [ Reset ]
        mapM_ (prettyPrintBlock width) bs
prettyPrintBlock width (Blockquote bs) = do
    forM_ bs $ \b -> do
        setSGR [ SetColor Foreground Vivid Black ]
        putStr ("  > ")
        setSGR [ SetColor Foreground Vivid Blue ]
        prettyPrintBlock width b
prettyPrintBlock width (CodeBlock _ t) = do
    setSGR [ SetColor Foreground Dull Yellow ]
    forM_ (Text.lines t) $ \l -> do
        Text.putStrLn ("    " <> l)
    setSGR [ Reset ]
prettyPrintBlock width HRule = do
    setSGR [ SetColor Foreground Vivid Black ]
    putStr (take width (repeat '-'))
    setSGR [ Reset ]
prettyPrintBlock width (HtmlBlock html) = do
    Text.putStrLn html
prettyPrintBlock width block = print block

prettyPrintInline :: Inline -> IO ()
prettyPrintInline (Str s) = Text.putStr s
prettyPrintInline (Link els url _) = do
    putChar '['
    forM_ els $ \el -> do
        setSGR [ SetConsoleIntensity BoldIntensity ]
        prettyPrintInline el
    setSGR [ Reset ]
    putChar ']'
    putChar '('
    setSGR [ SetColor Foreground Vivid Blue ]
    Text.putStr url
    setSGR [ Reset ]
    putChar ')'
prettyPrintInline Space = putStr " "
prettyPrintInline SoftBreak = putStr " "
prettyPrintInline (Emph els) = do
    forM_ els $ \el -> do
        setSGR [ SetItalicized True
               , SetUnderlining SingleUnderline
               ]
        prettyPrintInline el
    setSGR [ Reset ]
prettyPrintInline (Strong els) = do
    forM_ els $ \el -> do
        setSGR [ SetConsoleIntensity BoldIntensity ]
        prettyPrintInline el
    setSGR [ Reset ]
prettyPrintInline (Code s) = do
    setSGR [ SetColor Foreground Dull Yellow ]
    Text.putStr s
    setSGR [ Reset ]
prettyPrintInline el = print el
