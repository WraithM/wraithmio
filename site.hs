{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad ((>=>))
import           Hakyll


main :: IO ()
main = hakyll $ do
    match "images/*" copyFiles
    match "js/*" copyFiles

    match "css/*.css" $ do
        route idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    match (fromList ["resume.md", "contact.md", "about.md"]) $ do
        route $ setExtension "html"
        compile $ pandocTemplate "templates/other.html" defaultContext

    match "posts/*" postTemplate
    match "drafts/*" postTemplate

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"

            let archiveCtx = listField "posts" postCtx (return posts) <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= relativizeDefault

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"

            let indexCtx = listField "posts" postCtx (return posts) <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= relativizeDefault


copyFiles :: Rules ()
copyFiles = do
    route idRoute
    compile copyFileCompiler


postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext


postTemplate :: Rules ()
postTemplate = do
    route $ setExtension "html"
    compile $ pandocTemplate "templates/post.html" postCtx


pandocTemplate :: Identifier -> Context String -> Compiler (Item String)
pandocTemplate tmpl ctx = demotedPandoc >>= loadAndApplyTemplate tmpl ctx >>= relativizeDefault
  where
    demotedPandoc :: Compiler (Item String)
    demotedPandoc = fmap demoteHeaders <$> pandocCompiler


relativizeDefault :: Item String -> Compiler (Item String)
relativizeDefault = loadAndApplyTemplate "templates/default.html" defaultContext >=> relativizeUrls
