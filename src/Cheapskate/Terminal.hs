{-# LANGUAGE OverloadedStrings #-}
module Cheapskate.Terminal
  where

import           Cheapskate
import           Control.Monad                       (forM_)
import           Data.Monoid
import qualified Data.Text                           as Text
import qualified Data.Text.IO                        as Text
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise (readColourPrefs)
import           System.Console.ANSI
import           System.Console.Terminal.Size        (Window (..), size)
import           System.Directory
import           Text.Highlighting.Pygments

prettyPrint :: Doc -> IO ()
prettyPrint (Doc _ blocks) = do
    putStrLn ""
    wid <- size >>= \s -> case s of
        Just (Window _ w) -> return w
        Nothing -> return 80
    forM_ blocks $ \block -> do
        prettyPrintBlock wid block
        putStrLn ""

prettyPrintBlock :: Int -> Block -> IO ()
prettyPrintBlock _ (Header level els) = do
    setSGR [ SetColor Foreground Vivid Black
           , SetConsoleIntensity BoldIntensity
           ]
    putStr (replicate level '#')
    putStr " "
    setSGR [ Reset ]
    setSGR [ SetColor Foreground Vivid Cyan
           , SetConsoleIntensity BoldIntensity
           ]
    mapM_ prettyPrintInline els
    setSGR [ Reset ]
    putStrLn ""
prettyPrintBlock _ (Para els) = do
    mapM_ prettyPrintInline els
    putStrLn ""
prettyPrintBlock wid (List _ (Bullet c) bss) = forM_ bss $ \bs -> do
    setSGR [ SetColor Foreground Vivid Black ]
    putStr ("  " ++ (c:" "))
    setSGR [ Reset ]
    mapM_ (prettyPrintBlock wid) bs
prettyPrintBlock wid (List _ (Numbered w i) bss) =
    forM_ ibss $ \(bs, j) -> do
        setSGR [ SetColor Foreground Vivid Black ]
        let wc = case w of
                PeriodFollowing -> '.'
                ParenFollowing -> ')'
        putStr ("  " ++ show (i + j) ++ (wc : " "))
        setSGR [ Reset ]
        mapM_ (prettyPrintBlock wid) bs
  where
    ibss = zip bss [0..]
prettyPrintBlock wid (Blockquote bs) = forM_ bs $ \b -> do
    setSGR [ SetColor Foreground Vivid Black ]
    putStr "  > "
    setSGR [ SetColor Foreground Vivid Blue ]
    prettyPrintBlock wid b
prettyPrintBlock _ (CodeBlock (CodeAttr "" _) t) = do
    setSGR [ SetColor Foreground Dull Yellow ]
    forM_ (Text.lines t) $ \l -> Text.putStrLn ("    " <> l)
    setSGR [ Reset ]
prettyPrintBlock _ (CodeBlock (CodeAttr "haskell" _) t) = do
    prefs <- readColourPrefs
    let code = hscolour TTY prefs False True "" False (Text.unpack t)
    forM_ (lines code) $ \l -> putStrLn ("    " <> l)
prettyPrintBlock wid (CodeBlock (CodeAttr lang info) t) = do
    mlexer <- findLexer
    case mlexer of
        Nothing -> prettyPrintBlock wid (CodeBlock (CodeAttr "" info) t)
        Just lexer -> do
            highlighted <- highlight lexer terminalFormatter [] (Text.unpack t)
            forM_ (lines highlighted) $ \l -> putStrLn ("    " <> l)
  where
    findLexer = do
        mpygments <- findExecutable "pygmentize"
        case mpygments of
            Nothing -> return Nothing
            Just _ -> getLexerByName (Text.unpack lang)
prettyPrintBlock wid HRule = do
    setSGR [ SetColor Foreground Vivid Black ]
    putStr (replicate wid '-')
    setSGR [ Reset ]
prettyPrintBlock _ (HtmlBlock html) = Text.putStrLn html

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
