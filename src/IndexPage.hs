{-
    Модуль, отвечающий за формирование главной страницы.
    https://github.com/denisshevchenko/blog
    Все права принадлежат Денису Шевченко, 2011-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module IndexPage (
    createIndexPage
) where

import Data.Monoid          (mconcat)
import Context              (postContext)
import Misc                 (TagsReader)
import Control.Monad.Reader
import Hakyll

createIndexPage :: TagsReader
createIndexPage = do
    tags <- ask
    lift $ create ["index.html"] $ do
        route idRoute
        compile $ do
            -- На главной странице будет отражено 7 последних публикаций.
            last7Posts <- fmap (take 7) . recentFirst =<< loadAll "posts/**"
            let indexContext = mconcat [ listField "posts" (postContext tags) (return last7Posts) 
                                       , constField "title" "Мысли и опыт"
                                       , defaultContext
                                       ]
            
            makeItem "" >>= loadAndApplyTemplate "templates/index.html" indexContext
                        >>= loadAndApplyTemplate "templates/default.html" indexContext
                        >>= relativizeUrls
    return ()

