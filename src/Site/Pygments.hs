{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Pygments (
  pygmentsServer,
  pygments
) where

import Site.Types

import Hakyll

import qualified Data.Text as T

import Text.Pandoc
import Text.Pandoc.Walk (walkM)

import Control.Monad ((>=>))

import Data.Maybe (fromMaybe, fromJust)

import qualified System.IO.Streams as S
import System.IO.Streams.Process (runInteractiveProcess)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as U8

import Text.Blaze.Html (preEscapedToHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Fancydiff.Lib               (getHighlighterFunc,
                                              stringToHighlighter)
import           Fancydiff.HTMLFormatting    ( htmlFormatting
                                             , mkHtmlFormat
                                             , HTMLStyle(..)
                                             )
import           Fancydiff.Themes            (brightBackground)
import           Fancydiff.Formatting        (combineFLists, applyMarkers,
                                              flistToText, makeFreeForm)

pygmentsServer :: IO Streams
pygmentsServer = do
  (inp, out, _, _) <- runInteractiveProcess "python" ["src/pig.py"] Nothing Nothing
  return (inp, out)

pygments :: Streams -> Pandoc -> Compiler Pandoc
pygments streams = walkM (generateCodeBlock streams)

generateCodeBlock :: Streams -> Block -> Compiler Block
generateCodeBlock streams (CodeBlock (_, classes, keyvals) contents) = do
  let lang = fromMaybe (if null classes then "text" else head classes) $ lookup "lang" keyvals
      lineNumbers = fromMaybe False $ fmap (/= "") $ lookup "lineNumbers" keyvals

      markLookup i fieldName = lookup ("mark" ++ show i ++ fieldName) keyvals
      marks = markN (1 :: Int)
      markN idx = case (markLookup idx "Start", markLookup idx "End",
                        markLookup idx "Class") of
                      (Just l, Just r, Just c) -> (l, r, c):(markN (idx+1))
                      _ -> []

      title = lookup "title" keyvals

      onlyPygments = do
          code <- if lang == "text"
                    then return $ renderHtml $ H.toHtml contents
                    else pygmentize streams lang contents

          let colored = renderHtml $ H.pre $ H.code ! A.class_ (H.toValue $ "highlight language-" ++ lang) $ do
                          preEscapedToHtml code
              caption = maybe "" (renderHtml . H.figcaption . H.span . preEscapedToHtml) $ lookup "text" keyvals
              composed = renderHtml $ H.figure ! A.class_ "codeblock" $ do
                           preEscapedToHtml $ caption ++ colored

          return $ RawBlock "html" composed

      onlyFancydiff = do
          let highlighter = stringToHighlighter $ T.pack lang
              func = getHighlighterFunc highlighter
              textContent = T.pack contents

              -- addMarks t [] = (Right $ mkPlain t, t)
              -- addMarks t ((l, r, c):xs) = (root, textWithoutMarkers)
              --     where
              --         (previousMarkers, textContent) = addMarks t xs
              --         flistWithMarkers =
              --             applyMarkers (T.pack l) (T.pack r) (makeFreeForm $ T.pack c) textContent
              --         textWithoutMarkers = flistToText flistWithMarkers
              --         root = combineFLists textWithoutMarkers flistWithMarkers previousMarkers
              --
              -- Cannot use applyMarkers recursively. Need to be able to implement a 'delete' operation
              -- on FList based on the removals in applyMarkers.

              maybeHighlighted =
                  case marks of
                      [] -> func textContent
                      ((l, r, c):_) ->
                          let flistWithMarkers =
                                  applyMarkers (T.pack l) (T.pack r) (makeFreeForm $ T.pack c)
                                     textContent
                              textWithoutMarkers = flistToText flistWithMarkers
                           in case func textWithoutMarkers of
                                Left err -> Left err
                                Right highlighted ->
                                    Right $ combineFLists textWithoutMarkers highlighted flistWithMarkers

              addLineNumbersF t = T.concat
                          [ "<table class=\"codeBox\"><tbody>"
                          , titleText
                          , "<tr>"
                          , numbersText
                          , "<td class=\"sourceCode ", lineWrap, "\">"
                          , t
                          , "</td></tr></tbody></table>"
                          ]
                    where
                      titleText =
                          case title of
                             Just t2 -> T.concat
                                  [ "<tr><td class=\"codeTitle\">"
                                  , T.pack t2
                                  , "</td></tr><tr></tr>"
                                  ]
                             Nothing -> ""
                      numbersText =
                          case lineNumbers of
                             True -> T.concat
                                  [ "<td class=\"lineNumbers\"><div class=\"lineNumbersDiv\"><pre>"
                                  , T.unlines $ map (T.pack . show) [1..(length $ T.lines t)]
                                  , "</div></pre></td>"
                                  ]
                             False -> ""
                      lineWrap =
                          case lineNumbers of
                             True -> ""
                             False -> "sourceCodeWrap"

          case maybeHighlighted of
              Left _ -> onlyPygments
              Right highlighted -> do
                  let withoutLineNumbers =
                          htmlFormatting (mkHtmlFormat HTMLSCSS brightBackground) highlighted
                      final = T.unpack $ T.concat
                        [ "<div class=\"fancydiff-block\">"
                        , addLineNumbersF withoutLineNumbers
                        , "</div>"
                        ]
                  return $ RawBlock "html" $ final

  case lookup "fancydiff" keyvals of
      Nothing -> onlyPygments
      Just _ -> onlyFancydiff

generateCodeBlock _ x = return x

pygmentize :: Streams -> String -> String -> Compiler String
pygmentize (os, is) lang contents = unsafeCompiler $ do
  let lang'     = U8.fromString lang
      contents' = U8.fromString contents
      len       = U8.fromString . show . U8.length $ contents'

      -- REQUEST:  LANG\nLENGTH\nCODE
      request = C.intercalate "\n" [lang', len, contents']

  mapM_ (flip S.write os) $ map Just [request, ""]

  -- RESPONSE: LENGTH\nRESPONSE
  responseLength <- read . U8.toString . fromJust <$> (S.lines >=> S.read) is
  U8.toString <$> S.readExactly responseLength is

