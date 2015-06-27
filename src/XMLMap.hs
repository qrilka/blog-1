{-
    Модуль, отвечающий за построение XML-карты сайта.
    https://github.com/denisshevchenko/blog
    Все права принадлежат Денису Шевченко, 2011-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module XMLMap (
    createXMLMap
) where

import Data.Monoid          (mconcat)
import Context              (postContext)
import Misc                 (aHost)
import Misc                 (TagsReader)
import Control.Monad.Reader
import Hakyll

createXMLMap :: TagsReader
createXMLMap = do
    tags <- ask
    lift $ create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            allPosts <- recentFirst =<< loadAll "posts/**"
            let sitemapContext = mconcat [ listField "entries" (postContext tags) (return allPosts)
                                         , constField "host" aHost
                                         , defaultContext
                                         ]
            -- Формируем страницу sitemap.xml, применяя к ней шаблон и контекст.
            makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapContext
    return ()
