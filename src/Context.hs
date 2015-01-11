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
import Misc             (aHost)
import Hakyll

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
postContext :: Tags -> Context String
postContext tags = mconcat [ constField "host" aHost
                           , dateFieldWith ruTimeLocale "date" "%d %B %Y"
                           , tagsField "postTags" $ tags
                           , defaultContext
                           ]

