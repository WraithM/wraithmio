{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import           Hakyll


main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route idRoute
        compile copyFileCompiler

    match "sass/default.scss" $ do
        route $ constRoute "css/default.css"
        compile compressScssCompiler

    match "css/*.css" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList ["resume.md", "contact.md", "about.md"]) $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/other.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"

            let archiveCtx = listField "posts" postCtx (return posts) <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"

            let indexCtx = listField "posts" postCtx (return posts) <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext


compressScssCompiler :: Compiler (Item String)
compressScssCompiler = fmap compressCss <$> (getResourceString >>= withItemBody runSass)
  where
    runSass = unixFilter "sass"
        [ "-s"
        , "--scss"
        , "--style", "compressed"
        , "--load-path", "sass"
        ]
