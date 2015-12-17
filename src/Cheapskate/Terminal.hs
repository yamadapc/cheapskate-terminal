{-# LANGUAGE OverloadedStrings #-}
module Cheapskate.Terminal
  where

import Data.List
import Data.Foldable
import           Cheapskate
import           Control.Monad                       (void, forM_)
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
prettyPrintBlock _ (Header level els) = {-# SCC prettyPrintBlockHeader #-} do
    setSGR [ SetColor Foreground Vivid Black
           , SetConsoleIntensity BoldIntensity
           ]
    putStr (replicate level '#')
    putStr " "
    setSGR [ Reset ]
    setSGR [ SetColor Foreground Vivid Cyan
           , SetConsoleIntensity BoldIntensity
           ]
    executeSgrs (concatMap prettyPrintInline els)
    setSGR [ Reset ]
    putStrLn ""
prettyPrintBlock _ (Para els) = {-# SCC prettyPrintBlockPara #-} do
    -- map prettyPrintInline els
    executeSgrs (concatMap prettyPrintInline els)
    putStrLn ""
prettyPrintBlock wid (List _ (Bullet c) bss) = {-# SCC prettyPrintBlockListBullet #-} forM_ bss $ \bs -> do
    setSGR [ SetColor Foreground Vivid Black ]
    putStr ("  " ++ (c:" "))
    setSGR [ Reset ]
    mapM_ (prettyPrintBlock wid) bs
prettyPrintBlock wid (List _ (Numbered w i) bss) = {-# SCC prettyPrintBlockListNumbered #-}
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
prettyPrintBlock wid (Blockquote bs) = {-# SCC prettyPrintBlockBlockquote #-} forM_ bs $ \b -> do
    setSGR [ SetColor Foreground Vivid Black ]
    putStr "  > "
    setSGR [ SetColor Foreground Vivid Blue ]
    prettyPrintBlock wid b
prettyPrintBlock _ (CodeBlock (CodeAttr "" _) t) = {-# SCC prettyPrintBlockCodeBlock #-} do
    setSGR [ SetColor Foreground Dull Yellow ]
    forM_ (Text.lines t) $ \l -> Text.putStrLn ("    " <> l)
    setSGR [ Reset ]
prettyPrintBlock _ (CodeBlock (CodeAttr "haskell" _) t) = {-# SCC prettyPrintBlockCodeBlockHaskell #-} do
    prefs <- readColourPrefs
    let code = hscolour TTY prefs False True "" False (Text.unpack t)
    forM_ (lines code) $ \l -> putStrLn ("    " <> l)
prettyPrintBlock wid (CodeBlock (CodeAttr lang info) t) = {-# SCC prettyPrintBlockCodeBlockPygments #-} do
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
prettyPrintBlock wid HRule = {-# SCC prettyPrintBlockHRule #-} do
    setSGR [ SetColor Foreground Vivid Black ]
    putStr (replicate wid '-')
    setSGR [ Reset ]
prettyPrintBlock _ (HtmlBlock html) = {-# SCC prettyPrintBlockHtmlBlock #-} Text.putStrLn html

{-# INLINE executeSgrs #-}
executeSgrs as = do
    _ <- foldlM go [] (foldr merge [] as)
    return ()
  where
    {-# INLINE merge #-}
    merge (sgr', t') ((sgr, t):ms) | sgr == sgr' = (sgr, t' <> t):ms
    merge a m = a:m

    {-# INLINE go #-}
    go m ([], "") = return m
    go m ([], t) = do
        Text.putStr t
        return m
    go m (sgrs, "") = do
        m' <- setSGR' m sgrs
        return m'
    go m (sgrs, t) = do
        m' <- setSGR' m sgrs
        Text.putStr t
        return m'
    setSGR' m sgrs = case sgrs \\ m of
        [] -> return m
        a -> do
            setSGR a
            return (m ++ a)

prettyPrintInline :: Inline -> [([SGR], Text.Text)]
prettyPrintInline (Str s) = {-# SCC prettyPrintInlineStr #-} [([], s)]
prettyPrintInline (Link els url _) = {-# SCC prettyPrintInlineLink #-} -- do
    ([], "[") :
    -- map (\(sgrs, t) -> ((SetConsoleIntensity BoldIntensity):sgrs, t))
    ([ SetConsoleIntensity BoldIntensity ], "") :
    (map (\(sgrs, t) -> ((SetConsoleIntensity BoldIntensity):sgrs, t))
     (concat (toList (fmap prettyPrintInline els)))) ++
    -- forM_ els $ \el -> do
    --     setSGR [ SetConsoleIntensity BoldIntensity ]
    --     prettyPrintInline el
    -- setSGR [ Reset ]
    [ ([ Reset ], "](")
    , ([ SetColor Foreground Vivid Blue ], url)
    , ([ Reset ], ")")
    ]
    -- putChar ']'
    -- putChar '('
    -- setSGR [ SetColor Foreground Vivid Blue ]
    -- Text.putStr url
    -- setSGR [ Reset ]
    -- putChar ')'
prettyPrintInline Space = {-# SCC prettyPrintInlineSpace #-} [([], " ")]
prettyPrintInline SoftBreak = {-# SCC prettyPrintInlineSoftBreak #-} [([], " ")]
prettyPrintInline (Emph els) = {-# SCC prettyPrintInlineEmph #-} -- do
    (map (\(sgrs, t) -> ((SetItalicized True):(SetUnderlining SingleUnderline):sgrs, t))
     (concat (toList (fmap prettyPrintInline els)))) ++
    -- forM_ els $ \el -> do
    --     setSGR [ SetItalicized True
    --            , SetUnderlining SingleUnderline
    --            ]
    --     prettyPrintInline el
    [([Reset], "")]
    -- setSGR [ Reset ]
prettyPrintInline (Strong els) = {-# SCC prettyPrintInlineStrong #-} -- do
    (map (\(sgrs, t) -> ((SetConsoleIntensity BoldIntensity):sgrs, t))
     (concat (toList (fmap prettyPrintInline els))))
    -- forM_ els $ \el -> do
    --     setSGR [ SetConsoleIntensity BoldIntensity ]
    --     prettyPrintInline el
    -- setSGR [ Reset ]
prettyPrintInline (Code s) = {-# SCC prettyPrintInlineCode #-} -- do
    ([SetColor Foreground Dull Yellow], s):
    [([Reset], "")]
    -- setSGR [ SetColor Foreground Dull Yellow ]
    -- Text.putStr s
    -- setSGR [ Reset ]
prettyPrintInline el = {-# SCC prettyPrintInlineUnknown #-} [([], Text.pack (show el))] -- print el
