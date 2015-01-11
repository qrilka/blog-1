{-
    Вспомогательный модуль.
    https://github.com/denisshevchenko/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Misc (
    aHost,
    prepareAllTemplates,
    getNameOfAuthor,
    TagsAndAuthors,
    TagsReader,
    getRussianNameOfCategory
) where

import Control.Monad.Reader
import qualified Data.Map as M
import Hakyll

-- Данный URL останется актуальным до тех пор, пока сайт будет жить на GitHub Pages.
aHost :: String
aHost = "http://haskell.dshevchenko.biz"

-- Готовим все шаблоны из каталога templates.
prepareAllTemplates :: Rules ()
prepareAllTemplates = match "templates/*" $ compile templateCompiler

-- Читательское "облако" с тематическими тегами, категориями и именами авторов статей.
type TagsAndAuthors = [Tags]
type TagsReader = ReaderT TagsAndAuthors Rules ()

-- Извлекает из статьи значение атрибута `author`.
getNameOfAuthor :: MonadMetadata m => Identifier -> m [String]
getNameOfAuthor identifier = do
    -- Собираем атрибуты статьи в обычный ассоциативный контейнер.
    metadata <- getMetadata identifier
    let maybeAuthor = M.lookup "author" metadata
    return $ case maybeAuthor of
        -- Поразумевается, что у статьи всегда один автор, а не несколько.
        Nothing -> ["Не указан"]
        Just nameOfAuthor -> [trim nameOfAuthor]

-- Имена категорий извлекаются из файлового пути, поэтому они всегда английские.
-- Это не очень красиво, поэтому мы формируем словарь русских имён для категорий.
russianNamesOfCategories :: M.Map String String
russianNamesOfCategories = M.fromList[ ("web",      "Веб")
                                     , ("tasks",    "Задачи")
                                     , ("projects", "Проекты")
                                     ]

getRussianNameOfCategory :: String -> String
getRussianNameOfCategory englishName = 
    case (M.lookup englishName russianNamesOfCategories) of
        Nothing   -> englishName
        Just name -> name

