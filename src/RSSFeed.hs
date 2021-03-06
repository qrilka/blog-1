{-
    Модуль, отвечающий за работу с RSS. 
    https://github.com/denisshevchenko/blog
    Все права принадлежат Денису Шевченко, 2011-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module RSSFeed (
    setupRSSFeed
) where

import Data.Monoid          (mconcat)
import Misc                 (aHost)
import Context              (postContext)
import Misc                 (TagsReader)
import Control.Monad.Reader
import Hakyll

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration { feedTitle       = "Д. Шевченко"
                                      , feedDescription = "Мысли и опыт"
                                      , feedAuthorName  = "Денис Шевченко"
                                      , feedAuthorEmail = "me@dshevchenko.biz"
                                      , feedRoot        = aHost
                                      }

-- Формируем стандартную RSS-ленту, на основе последних 10 публикаций.
setupRSSFeed :: TagsReader
setupRSSFeed = do
    tags <- ask
    lift $ create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedContext = mconcat [ postContext tags
                                      , constField "description" ""
                                      ]
            -- Учитываем 10 последних статей.
            last10Posts <- fmap (take 10) . recentFirst =<< loadAll "posts/**"
            renderRss feedConfiguration 
                      feedContext 
                      last10Posts
    return ()

