{-
    Модуль, отвечающий за преобразование статей и в формирование корректных путей к ним.
    https://github.com/denisshevchenko/blog
    Все права принадлежат Денису Шевченко, 2011-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Posts (
    createPosts
) where

import Data.List.Split      (splitOn)
import Data.List            (intersperse)
import System.FilePath      (splitExtension)
import Context              (postContext)
import Misc                 (TagsReader)
import Control.Monad.Reader
import Hakyll

removePostsDirectoryFromURLs :: Routes
removePostsDirectoryFromURLs = gsubRoute "posts/" (const "")

-- Дата публикации будет отражена в URL в виде подкаталогов.
directorizeDate :: Routes
directorizeDate = customRoute (\i -> directorize $ toFilePath i)
    where
        directorize path = dirs 
            where
                (dirs, _) = splitExtension $ 
                            concat $
                            (intersperse "/" date) ++ ["/"] ++ (intersperse "-" rest)
                minusBetweenDateAndTitle = 3
                (date, rest) = splitAt minusBetweenDateAndTitle $ splitOn "-" path

createPosts :: TagsReader
createPosts = do
    tagsAndAuthors <- ask
    -- Берём все файлы из каталога posts.
    lift $ match "posts/**" $ do
        route $ removePostsDirectoryFromURLs `composeRoutes`
                directorizeDate `composeRoutes`
                setExtension "html"
        -- Для превращения Markdown в HTML используем pandocCompiler.
        compile $ pandocCompiler >>= loadAndApplyTemplate "templates/post.html" (postContext tagsAndAuthors)
                                 >>= loadAndApplyTemplate "templates/default.html" (postContext tagsAndAuthors)
                                 >>= relativizeUrls
    return ()

