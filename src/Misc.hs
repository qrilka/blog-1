{-
    Вспомогательный модуль.
    https://github.com/denisshevchenko/blog
    Все права принадлежат Денису Шевченко, 2011-2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Misc (
    aHost,
    prepareAllTemplates,
    TagsReader
) where

import Control.Monad.Reader
import qualified Data.Map as M
import Hakyll

-- Данный URL останется актуальным до тех пор, пока сайт будет жить на GitHub Pages.
aHost :: String
aHost = "http://blog.dshevchenko.biz"

-- Готовим все шаблоны из каталога templates.
prepareAllTemplates :: Rules ()
prepareAllTemplates = match "templates/*" $ compile templateCompiler

-- Читательское "облако" с тематическими тегами.
type TagsReader = ReaderT Tags Rules ()

