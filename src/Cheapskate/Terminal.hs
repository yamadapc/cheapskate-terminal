{-# LANGUAGE OverloadedStrings #-}
module Cheapskate.Terminal
  where

import           Cheapskate
import           Control.Monad                       (foldM_, forM_)
import           Data.Foldable                       (toList)
import           Data.Maybe                          (isJust)
import           Data.Monoid
import           Data.String                         (fromString)
import qualified Data.Text                           as Text
import           Data.Text.Internal.Builder          (fromText, toLazyText)
import qualified Data.Text.IO                        as Text
import qualified Data.Text.Lazy                      as Text.Lazy
import qualified Data.Text.Lazy.IO                   as Text.Lazy
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise (ColourPrefs,
                                                      readColourPrefs)
import           System.Console.ANSI
import           System.Console.ANSI.Builder
import           System.Console.Terminal.Size        (Window (..), size)
import           System.Directory
import           System.IO
import           Text.Highlighting.Pygments

prettyPrint :: Doc -> IO ()
prettyPrint (Doc _ blocks) = do
    putStrLn ""
    hasPygments <- isJust <$> findExecutable "pygmentize"
    prefs <- readColourPrefs
    wid <- size >>= \s -> case s of
        Just (Window _ w) -> return w
        Nothing -> return 80
    forM_ blocks $ \block -> do
        let Builder s = (prettyPrintBlock hasPygments prefs wid block)
        foldM_ (\m (b, sgr) -> do
            setSGR sgr

            let ws = Text.Lazy.words (toLazyText b)
                (tw, tw') = snd (foldl (\(l, (t, t')) w -> if l > 80 then (0, (t, t' <> [w])) else (l, (t <> [w], t'))) (0, ([], [])) ws)
                (t, t') = (Text.Lazy.unwords tw, Text.Lazy.unwords tw')
            if not (Text.Lazy.null t')
                then do
                    Text.Lazy.putStr t
                    putStrLn ""
                    Text.Lazy.putStr (Text.Lazy.stripStart t')
                    return (Text.Lazy.length t')
                else do
                    Text.Lazy.putStr t
                    return (m + Text.Lazy.length t)) 0 s
        putStrLn ""

prettyPrintBlock :: Bool -> ColourPrefs -> Int -> Block -> Builder
prettyPrintBlock _ _ _ (Header level els) =
    bsgr [ SetColor Foreground Vivid Black
         , SetConsoleIntensity BoldIntensity
         ] <>
    bstr (fromString (replicate level '#')) <>
    " " <>
    bsgr [ Reset ] <>
    bsgr [ SetColor Foreground Vivid Cyan
         , SetConsoleIntensity BoldIntensity
         ] <>
    mconcat (map prettyPrintInline (toList els)) <>
    bsgr [ Reset ] <>
    "\n"
prettyPrintBlock _ _ _ (Para els) =
    mconcat (map prettyPrintInline (toList els)) <> bstr "\n"
prettyPrintBlock hasPygments prefs wid (List _ (Bullet c) bss) =
    mconcat (map helper (toList bss))
  where
    helper bs =
        bsgr [ SetColor Foreground Vivid Black ] <>
        fromString ("  " ++ (c:" ")) <>
        bsgr [ Reset ] <>
        mconcat (map (prettyPrintBlock hasPygments prefs wid) (toList bs))
prettyPrintBlock hasPygments prefs wid (List _ (Numbered w i) bss) =
    mconcat (map helper ibss)
  where
    helper (bs, j) =
        bsgr [ SetColor Foreground Vivid Black ] <>
        (let wc = case w of
                PeriodFollowing -> '.'
                ParenFollowing -> ')'
         in fromString ("  " ++ show (i + j) ++ (wc : " "))) <>
        bsgr [ Reset ] <>
        mconcat (map (prettyPrintBlock hasPygments prefs wid) (toList bs))
    ibss = zip bss [0..]
prettyPrintBlock hasPygments prefs wid (Blockquote bs) =
    mconcat (map helper (toList bs))
  where
    helper b = bsgr [ SetColor Foreground Vivid Black ] <>
        "  > " <>
        bsgr [ SetColor Foreground Vivid Blue ] <>
        prettyPrintBlock hasPygments prefs wid b
prettyPrintBlock _ _ _ (CodeBlock (CodeAttr "" _) t) =
    bsgr [ SetColor Foreground Dull Yellow ] <>
    mconcat (map (bstr . (<> "\n") . ("    " <>) . fromText) (Text.lines t)) <>
    bsgr [ Reset ]
prettyPrintBlock _ prefs _ (CodeBlock (CodeAttr "haskell" _) t) =
    mconcat (map (bstr . (<> "\n") . ("    " <>) . fromText . Text.strip . Text.pack) (lines code))
  where
    code = hscolour TTY prefs False True "" False (Text.unpack t)
-- prettyPrintBlock True prefs wid (CodeBlock (CodeAttr lang info) t) =
--     case getLexerByName lang of
--         Nothing -> prettyPrintBlock True prefs wid (CodeBlock (CodeAttr "" info) t)
--         Just lexer -> do
--             highlighted <- highlight lexer terminalFormatter [] (Text.unpack t)
--             map
--                 (lines highlighted) $ \l -> putStrLn ("    " <> l)
prettyPrintBlock _ _ wid HRule =
    bsgr [ SetColor Foreground Vivid Black ] <>
    bstr (fromString (replicate wid '-')) <>
    bsgr [ Reset ]
prettyPrintBlock _ _ _ (HtmlBlock html) = bstr (fromText html) <> "\n"

prettyPrintInline :: Inline -> Builder
prettyPrintInline (Str s) = bstr (fromText s)
prettyPrintInline (Link els url _) =
    "[" <>
    mconcat (flip map (toList els) $ \el ->
        bsgr [ SetConsoleIntensity BoldIntensity ] <>
        prettyPrintInline el) <>
    bsgr [ Reset ] <>
    "](" <>
    bsgr [ SetColor Foreground Vivid Blue ] <>
    bstr (fromText url) <>
    bsgr [ Reset ] <>
    bstr ")"
prettyPrintInline Space = bstr " "
prettyPrintInline SoftBreak = bstr " "
prettyPrintInline (Emph els) =
    (mconcat $ flip map (toList els) $ \el ->
        bsgr [ SetItalicized True
             , SetUnderlining SingleUnderline
             ] <>
        prettyPrintInline el) <>
    bsgr [ Reset ]
prettyPrintInline (Strong els) =
    (mconcat $ flip map (toList els) $ \el ->
        bsgr [ SetConsoleIntensity BoldIntensity ] <>
        prettyPrintInline el) <>
    bsgr [ Reset ]
prettyPrintInline (Code s) =
    bsgr [ SetColor Foreground Dull Yellow ] <>
    bstr (fromText s) <>
    bsgr [ Reset ]
prettyPrintInline (Image _ url tit) =
    bstr "![" <>
    bsgr [ SetConsoleIntensity BoldIntensity ] <>
    bstr (fromText tit) <>
    bsgr [ Reset ] <>
    bstr "](" <>
    bsgr [ SetColor Foreground Vivid Blue ] <>
    bstr (fromText url) <>
    bsgr [ Reset ] <>
    bstr ")"
prettyPrintInline _ = error "Not implemented"
