{-
    Главный модуль.
    https://github.com/denisshevchenko/blog
    Все права принадлежат Денису Шевченко, 2011-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Copiers              (justCopy, justCreateAndCopy, justCompressAndCopy)
import RSSFeed              (setupRSSFeed)
import Posts                (createPosts)
import Tags                 (createPageWithAllTags,
                             convertTagsToLinks,
                             buildPostsTags)
import XMLMap               (createXMLMap)
import Archive              (createPageWithAllPosts)
import Misc                 (prepareAllTemplates)
import IndexPage            (createIndexPage)
import Control.Monad.Reader (runReaderT)
import Hakyll

main :: IO ()
main = hakyll $ do
    justCopy            "static/images/*"
    justCompressAndCopy "static/css/*"
    justCopy            "README.md"
    justCopy            "CNAME"
    justCreateAndCopy   ".nojekyll"
    
    prepareAllTemplates
    
    -- Извлекаем названия тегов из всех публикаций.
    tags <- buildPostsTags

    -- Теги нужны всем, поэтому для удобства запускаем читателя.
    runReaderT (createPosts
                >> createPageWithAllPosts
                >> createPageWithAllTags
                >> convertTagsToLinks
                >> createXMLMap
                >> setupRSSFeed
                >> createIndexPage) tags

