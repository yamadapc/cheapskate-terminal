{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
module Cheapskate.Terminal
  where

import           Cheapskate
import           Control.Monad                       (foldM)
import           Data.Default
import           Data.Foldable                       (toList)
import           Data.Maybe                          (isJust)
import           Data.Monoid
import           Data.String                         (IsString)
import qualified Data.Text                           as Text (Text, lines, pack,
                                                              unpack)
import qualified Data.Text.Lazy                      as Text.Lazy
import           Data.Text.Lazy.Builder              (Builder, fromString,
                                                      fromText, toLazyText)
import qualified Data.Text.Lazy.IO                   as Text.Lazy
import           GHC.Int
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise (defaultColourPrefs,
                                                      readColourPrefs)
import           System.Console.ANSI
import           System.Console.Terminal.Size        (Window (..), size)
import           System.Directory
import           Text.Highlighting.Pygments

data PrettyPrintOptions = PrettyPrintOptions { prettyPrintWidth       :: Int64
                                             , prettyPrintColourPrefs :: ColourPrefs
                                             , prettyPrintHasPygments :: Bool
                                             }

instance Default PrettyPrintOptions where
    def = PrettyPrintOptions { prettyPrintColourPrefs = defaultColourPrefs
                             , prettyPrintWidth = 80
                             , prettyPrintHasPygments = False
                             }

prettyPrint :: Doc -> IO ()
prettyPrint = renderTerminal

renderTerminal :: Doc -> IO ()
renderTerminal doc = do
    b <- renderIO doc
    mapM_ Text.Lazy.putStrLn (Text.Lazy.lines b)

renderIO :: Doc -> IO Text.Lazy.Text
renderIO (Doc _ blocks) = do
    wid <- size >>= \s -> case s of
        Just (Window _ w) -> return w
        Nothing -> return 80
    prefs <- readColourPrefs
    hasPygments <- isJust <$> findExecutable "pygments"
    let opts = PrettyPrintOptions { prettyPrintWidth = wid
                                  , prettyPrintHasPygments = hasPygments
                                  , prettyPrintColourPrefs = prefs
                                  }
    foldM (helper opts) "\n" blocks
  where
    helper opts m block = do
        t <- renderBlock opts block
        return (m <> t <> "\n")

renderIOWith :: PrettyPrintOptions -> Doc -> IO Text.Lazy.Text
renderIOWith opts (Doc _ blocks) = foldM helper "\n" blocks
  where
    helper m block = do
        t <- renderBlock opts block
        return (m <> t <> "\n")

concats :: (IsString a, Monoid a) => [a] -> [a]
concats = scanl1 (\s v -> s <> " " <> v)

renderBlock :: PrettyPrintOptions -> Block -> IO Text.Lazy.Text
renderBlock opts@PrettyPrintOptions{..} b@(CodeBlock (CodeAttr lang _) t)
    | lang /= "haskell" && prettyPrintHasPygments = do
          mlexer <- getLexerByName (Text.unpack lang)
          case mlexer of
              Nothing -> return (renderBlockPure opts b)
              Just lexer -> do
                  let st = Text.unpack t
                  highlighted <- highlight lexer terminalFormatter [] st
                  return $ mconcatMapF ((<> "\n") . ("    " <>) . Text.Lazy.pack)
                                       (lines highlighted)
renderBlock opts block = return (renderBlockPure opts block)

renderBlockPure :: PrettyPrintOptions -> Block -> Text.Lazy.Text
renderBlockPure opts@PrettyPrintOptions{..} block = case block of
    (Header level els) ->
        setSGRCodeText [ SetColor Foreground Vivid Black
                    , SetConsoleIntensity BoldIntensity
                    ] <>
        Text.Lazy.replicate (fromIntegral level) "#" <> " " <>
        setSGRCodeText [ Reset ] <>
        setSGRCodeText [ SetColor Foreground Vivid Cyan
                    , SetConsoleIntensity BoldIntensity
                    ] <>
        toLazyText (mconcatMapF renderInline els) <>
        setSGRCodeText [ Reset ] <>
        "\n"
    (Para els) ->
        wordwrap prettyPrintWidth (toLazyText (mconcatMapF renderInline els))
    (List _ (Bullet c) bss) -> flip mconcatMapF bss $
        \bs -> setSGRCodeText [ SetColor Foreground Vivid Black ] <>
               Text.Lazy.pack ("  " ++ (c:" ")) <>
               setSGRCodeText [ Reset ] <>
               mconcatMapF (renderBlockPure opts) bs
    (List _ (Numbered w i) bss) ->
        let ibss = zip bss [0..]
        in flip mconcatMapF ibss $ \(bs, j) ->
        setSGRCodeText [ SetColor Foreground Vivid Black ] <>
        let wc = case w of
                PeriodFollowing -> '.'
                ParenFollowing -> ')'
         in Text.Lazy.pack ("  " ++ show (i + j) ++ (wc : " ")) <>
        setSGRCodeText [ Reset ] <>
        mconcatMapF (renderBlockPure opts) bs
    (Blockquote bs) -> flip mconcatMapF bs $ \b ->
        setSGRCodeText [ SetColor Foreground Vivid Black ] <>
        "  > " <>
        setSGRCodeText [ SetColor Foreground Vivid Blue ] <>
        renderBlockPure opts b
    (CodeBlock (CodeAttr "haskell" _) t) ->
        let code = hscolour
                TTY prettyPrintColourPrefs False True "" False (Text.unpack t)
        in toLazyText (mconcatMapF ((<> "\n") . ("    " <>) . fromString) (lines code))
    (CodeBlock (CodeAttr _ _) t) ->
        setSGRCodeText [ SetColor Foreground Dull Yellow ] <>
        mconcat (map (Text.Lazy.fromStrict . (<> "\n") . ("    " <>) ) (Text.lines t)) <>
        setSGRCodeText [ Reset ]
    HRule ->
        setSGRCodeText [ SetColor Foreground Vivid Black ] <>
        Text.Lazy.replicate (fromIntegral prettyPrintWidth) "-" <>
        setSGRCodeText [ Reset ]
    (HtmlBlock html) -> Text.Lazy.fromStrict html <> "\n"

renderInline :: Inline -> Builder
renderInline el = case el of
    LineBreak -> "\n"
    Space -> " "
    SoftBreak -> " "
    Entity t -> fromText t
    RawHtml t -> fromText t
    (Str s) -> fromText s
    (Link els url _) ->
        "[" <>
        renderInlinesWith [ SetConsoleIntensity BoldIntensity ] els <>
        setSGRCodeBuilder [ Reset ] <>
        "](" <>
        setSGRCodeBuilder [ SetColor Foreground Vivid Blue ] <>
        fromText url <>
        setSGRCodeBuilder [ Reset ] <>
        ")"
    (Emph els) ->
        renderInlinesWith [ SetItalicized True
                               , SetUnderlining SingleUnderline
                               ] els <>
        setSGRCodeBuilder [ Reset ]
    (Strong els) ->
        renderInlinesWith [ SetConsoleIntensity BoldIntensity ] els <>
        setSGRCodeBuilder [ Reset ]
    (Code s) ->
        setSGRCodeBuilder [ SetColor Foreground Dull Yellow ] <>
        fromText s <>
        setSGRCodeBuilder [ Reset ]
    (Image _ url tit) ->
        "![" <>
        setSGRCodeBuilder [ SetConsoleIntensity BoldIntensity ] <>
        fromText tit <>
        setSGRCodeBuilder [ Reset ] <>
        "](" <>
        setSGRCodeBuilder [ SetColor Foreground Vivid Blue ] <>
        fromText url <>
        setSGRCodeBuilder [ Reset ] <>
        ")"
  where
    renderInlinesWith sgr = mconcatMapF helper
      where
        helper e = setSGRCodeBuilder sgr <> renderInline e

wordwrap :: Int64 -> Text.Lazy.Text -> Text.Lazy.Text
wordwrap maxwidth = Text.Lazy.unlines . wordwrap' . Text.Lazy.words
  where
    wordwrap' [] = []
    wordwrap' ws = sentence : wordwrap' restwords
      where
        zipped = zip (concats ws) ws
        (sentences, rest) = span (\(s, _) -> Text.Lazy.length s <= maxwidth) zipped
        sentence = last (map fst sentences)
        restwords = map snd rest

setSGRCodeText :: [SGR] -> Text.Lazy.Text
setSGRCodeText = Text.Lazy.pack . setSGRCode

setSGRCodeTextS :: [SGR] -> Text.Text
setSGRCodeTextS = Text.pack . setSGRCode

setSGRCodeBuilder :: [SGR] -> Builder
setSGRCodeBuilder = fromText . setSGRCodeTextS

-- Probably there's a function in prelude that we don't know that does this
mconcatMapF :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
mconcatMapF f = mconcat . map f . toList
