{-# LANGUAGE OverloadedStrings #-}

import Hakyll hiding (pandocCompiler)

import Site.Filters
import Site.Fields
import Site.Routes
import Site.Pandoc

import Data.Monoid ((<>))
import GHC.IO.Encoding
import System.Environment
import Control.Monad (when)
import System.Directory

myHakyllConf :: Configuration
myHakyllConf = defaultConfiguration
  { deployCommand = "bash src/deploy.sh deploy"
  , providerDirectory = "provider"
  , destinationDirectory = "generated/deploy"
  , storeDirectory = "generated/cache"
  , tmpDirectory = "generated/cache/tmp"
  }

feedConf :: FeedConfiguration
feedConf = FeedConfiguration
  { feedTitle = "Jorge Israel Peña aka BLAENK DENUM"
  , feedDescription = "Personal Site"
  , feedAuthorName = "Jorge Israel Peña"
  , feedAuthorEmail = "jorge.israel.p@gmail.com"
  , feedRoot = "http://blaenkdenum.com"
  }

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  
  (action:_) <- getArgs

  -- establish configuration based on preview-mode
  let previewMode  = action == "preview"
      hakyllConf   = if previewMode
                     then myHakyllConf { destinationDirectory = "generated/preview" }
                     else myHakyllConf
      postsPattern = if previewMode
                     then "posts/*" .||. "drafts/*"
                     else "posts/*"

  -- cheap hack for preview edge-case
  when (action == "clean") $ do
    putStrLn "Removing generated/preview..."
    removeDirectoryRecursive "generated/preview"

  hakyllWith hakyllConf $ do
    tags <- buildTags postsPattern (fromCapture "tags/*.html")

    match ("images/**" .||. "font/*" .||. "js/*" .||. "static/**" .||. "favicon.png" .||. "CNAME") $ do
      route idRoute
      compile copyFileCompiler

    match "scss/screen.scss" $ do
      route $ gsubRoute "scss/" (const "css/") `composeRoutes` setExtension "css"
      compile $ sassCompiler

    match postsPattern $ do
      route $ niceRoute "posts/"
      compile $ getResourceBody
        >>= withItemBody (abbreviationFilter)
        >>= pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags <> postCtx)
        >>= loadAndApplyTemplate "templates/layout.html" postCtx

    match "pages/*" $ do
      route $ niceRoute ""
      compile $ getResourceBody
        >>= withItemBody (abbreviationFilter)
        >>= pandocCompiler
        >>= loadAndApplyTemplate "templates/page.html" postCtx
        >>= loadAndApplyTemplate "templates/layout.html" postCtx

    create ["404.html"] $ do
      route idRoute
      compile $ do
        makeItem ""
          >>= loadAndApplyTemplate "templates/404.html" defaultCtx
          >>= loadAndApplyTemplate "templates/layout.html" defaultCtx

    create ["index.html"] $ do
      route idRoute
      compile $ do
        makeItem ""
          >>= loadAndApplyTemplate "templates/index.html" (archiveCtx postsPattern)
          >>= loadAndApplyTemplate "templates/layout.html" defaultCtx

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx <> bodyField "description"
        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots postsPattern "content"
        renderAtom feedConf feedCtx posts

    niceTags tags $ \tag pattern -> do
      route $ niceRoute "tags/"
      compile $ do
        makeItem ""
          >>= loadAndApplyTemplate "templates/tags.html" (constField "tag" tag <> tagsCtx tags <> archiveCtx pattern)
          >>= loadAndApplyTemplate "templates/layout.html" defaultCtx

    match "templates/*" $ compile templateCompiler
