{-
    Модуль, отвечающий за формирование архива (списка всех статей).
    https://github.com/denisshevchenko/blog
    Все права принадлежат Денису Шевченко, 2011-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Archive (
    createPageWithAllPosts
) where

import Data.Monoid          (mconcat)
import Context              (postContext)
import Misc                 (TagsReader)
import Control.Monad.Reader
import Hakyll

createPageWithAllPosts :: TagsReader
createPageWithAllPosts = do
    tags <- ask
    lift $ create ["archive.html"] $ do
        route idRoute
        compile $ do
            allPosts <- recentFirst =<< loadAll "posts/**"
            let archiveContext = mconcat [ listField "posts" (postContext tags) (return allPosts) 
                                         , constField "title" "Архив"                   
                                         , defaultContext
                                         ]

            makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveContext
                        >>= loadAndApplyTemplate "templates/default.html" archiveContext
                        >>= relativizeUrls
    return ()

