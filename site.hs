--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>), mconcat)
import Data.List.Split (splitOn)
import Data.List (intersperse, isSuffixOf)
import System.FilePath (combine, splitExtension, takeFileName)

import Hakyll
--------------------------------------------------------------------------------

host :: String
host = "http://blog.dshevchenko.biz"

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ removePostsDirectoryFromURLs 
                `composeRoutes` 
                directorizeDate 
                `composeRoutes` 
                setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Архив"               <>
                    defaultContext

            makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                        >>= relativizeUrls

    create ["sitemap.xml"] $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let sitemapCtx = mconcat
                             [ listField "entries" postCtx (return posts)
                             , constField "host" host
                             , defaultContext
                             ]

            makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    
    -- tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    
    --create ["tags.html"] $ do
    --    route idRoute
    --    compile $ renderTagCloud 12.0 36.0 

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 7) . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Мысли и опыт"        <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

directorizeDate :: Routes
directorizeDate = customRoute (\i -> directorize $ toFilePath i)
    where
        directorize path = dirs 
            where
                (dirs, ext) = splitExtension $ 
                              concat $
                              (intersperse "/" date) ++ ["/"] ++ (intersperse "-" rest)
                (date, rest) = splitAt 3 $ splitOn "-" path

removePostsDirectoryFromURLs :: Routes
removePostsDirectoryFromURLs = gsubRoute "posts/" (const "")

postCtx :: Context String
postCtx = mconcat
    [ constField "host" host
    , dateField "date" "(%Y, %m, %d)"
    , defaultContext
    ]

