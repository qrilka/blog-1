{-
    Модуль, отвечающий за формирование базового контекста статей.
    https://github.com/denisshevchenko/blog
    Все права принадлежат Денису Шевченко, 2011-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Context (
    postContext
) where

import Data.Monoid      (mconcat)
import Data.List        (intersperse)
import System.Locale    
import Misc             (aHost, 
                         TagsAndAuthors,
                         getNameOfAuthor,
                         getRussianNameOfCategory)
import System.FilePath  (takeBaseName, takeDirectory)

import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import           Text.Blaze.Html                (toHtml, toValue, (!))

import Hakyll

-- Код данной функции для формирования простой ссылки взят из исходников Hakyll.
simpleRenderLink :: String 
                 -> (Maybe FilePath) 
                 -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
    -- Формируем тег <a href...>
    Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

-- Превращает имя автора в ссылку, ведущую к списку статей данного автора.
authorField :: String -> Tags -> Context a
authorField = tagsFieldWith getNameOfAuthor simpleRenderLink (mconcat . intersperse ", ")

-- Формируем ссылку, конвертируя "родное файловое" имя категории в русскоязычный аналог...
simpleRenderLinkForRussianCategory :: String 
                                   -> (Maybe FilePath) 
                                   -> Maybe H.Html
simpleRenderLinkForRussianCategory _   Nothing         = Nothing
simpleRenderLinkForRussianCategory tag (Just filePath) =
    -- Формируем тег <a href...>
    Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml (getRussianNameOfCategory tag)

-- Код данной функции, извлекающей имя категории из файлового пути, взят из исходников Hakyll.
getCategory :: MonadMetadata m => Identifier -> m [String]
getCategory = return . return . takeBaseName . takeDirectory . toFilePath

-- Превращает имя категории в русскоязычную ссылку, ведущую к списку статей, входящих в данную категорию.
categoryFieldInRussian :: String -> Tags -> Context a
categoryFieldInRussian = tagsFieldWith getCategory simpleRenderLinkForRussianCategory (mconcat . intersperse ", ")

-- Локализация в данном случае задаётся только для русских названий месяцев.
-- Остальные поля типа TimeLocale инициализированы пустыми значениями.
ruTimeLocale :: TimeLocale
ruTimeLocale =  TimeLocale { wDays  = []
                           , months = [("января",   "янв"),  ("февраля", "фев"),
                                       ("марта",    "мар"),  ("апреля",  "апр"),
                                       ("мая",      "мая"),  ("июня",    "июн"),
                                       ("июля",     "июл"),  ("августа", "авг"),
                                       ("сентября", "сент"), ("октября", "окт"),
                                       ("ноября",   "нояб"), ("декабря", "дек")]
                           , intervals = []
                           , amPm = ("", "")
                           , dateTimeFmt = "" 
                           , dateFmt = ""
                           , timeFmt = ""
                           , time12Fmt = ""
                           }

-- Основной контекст публикаций.
postContext :: TagsAndAuthors -> Context String
postContext tagsAndAuthors = mconcat [ constField "host" aHost
                                     , dateFieldWith ruTimeLocale "date" "%d %B %Y"
                                     , tagsField "postTags" $ tagsAndAuthors !! 0
                                     , categoryFieldInRussian "postCategory" $ tagsAndAuthors !! 1
                                     , authorField "postAuthor" $ tagsAndAuthors !! 2
                                     , defaultContext
                                     ]

