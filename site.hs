{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mconcat)
import Data.List.Split (splitOn)
import Data.List (intersperse, isSuffixOf)
import System.FilePath (combine, splitExtension, takeFileName)
import Network.HTTP (urlEncode)

import Hakyll

hostOfBlog :: String
hostOfBlog = "http://blog.dshevchenko.biz"

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration { feedTitle       = "Д. Шевченко"
                                        , feedDescription = "Мысли и опыт"
                                        , feedAuthorName  = "Денис Шевченко"
                                        , feedAuthorEmail = "me@dshevchenko.biz"
                                        , feedRoot        = hostOfBlog
                                        }

main :: IO ()
main = hakyll $ do
    -- Просто копируем все изображения из корневого каталога images...
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
    
    -- Просто копируем все стили из корневого каталога css...
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler 

    -- Просто копируем файл README
    match "README.md" $ do
        route   idRoute
        compile copyFileCompiler
    
    -- Просто копируем файл CNAME, необходимый для 
    -- поддержки собственного dns на GitHub Pages...
    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    -- Создаём файл .nojekyll и просто копируем его.
    -- Он необходим для того, чтобы сообщить GitHub Pages
    -- о том, что этот сайт (к сожалению или к счастью) не на Jekyll...
    create [".nojekyll"] $ do
        route   idRoute
        compile copyFileCompiler

    -- Создаём .htaccess и применяем к нему заготовленный шаблон...
    create [".htaccess"] $ do
        route idRoute
        compile $ makeItem "" >>= loadAndApplyTemplate "templates/htaccess" defaultContext

    -- Строим теги из заметок...
    -- urlEncode используется для корректного формирования не-ASCII меток... 
    tags <- buildTags "posts/*" (fromCapture "tags/*.html" . urlEncode)

    -- Обрабатываем все заметки...
    match "posts/*" $ do
        route $ removePostsDirectoryFromURLs `composeRoutes`
                directorizeDate `composeRoutes`
                setExtension "html"
        -- Используем pandocCompiler, потому что все заметки
        -- написаны на Markdown, и pandoc необходим, чтобы превратить их в html...
        compile $ pandocCompiler >>= loadAndApplyTemplate "templates/post.html" (postContext tags)
                                 >>= loadAndApplyTemplate "templates/default.html" (postContext tags)
                                 >>= relativizeUrls
    
    -- Создаём страницу 404 и применяем к ней шаблон стандартной страницы, а также собственную заготовку...
    create ["404.html"] $ do
        route idRoute
        compile $ makeItem "" >>= loadAndApplyTemplate "templates/404.html" (postContext tags)
                              >>= loadAndApplyTemplate "templates/default.html" (postContext tags)
                              >>= relativizeUrls

    -- Создаём страницу всех заметок...
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            -- Берём все заметки, начиная с самой поздней...
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveContext = mconcat [ listField "posts" (postContext tags) (return posts) 
                                         , constField "title" "Архив"                   
                                         , defaultContext
                                         ]

            makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveContext
                        >>= loadAndApplyTemplate "templates/default.html" archiveContext
                        >>= relativizeUrls

    -- Создаём стандартную XML-карту блога...
    create ["sitemap.xml"] $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let sitemapContext = mconcat [ listField "entries" (postContext tags) (return posts)
                                         , constField "host" hostOfBlog
                                         , defaultContext
                                         ]

            makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapContext 
    
    -- Подготавливаем "ссылочность" тегов: благодаря этому блоку
    -- каждый из тегов будет ссылкой, ведущей к списку заметок, 
    -- помеченных соответствующим тегом. Это относится как к облаку тегов,
    -- так и к персональным тегам в заметках.
    tagsRules tags $ \tag pattern -> do
        let title = "Все заметки по теме `" ++ tag ++ "`"
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let taggedPostsContext = mconcat [ listField "posts" (postContext tags) (return posts)
                                             , constField "title" title
                                             , defaultContext
                                             ]

            makeItem "" >>= loadAndApplyTemplate "templates/posts.html" taggedPostsContext 
                        >>= loadAndApplyTemplate "templates/default.html" taggedPostsContext
                        >>= relativizeUrls
    
    -- Настраиваем RSS feed...
    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedContext = mconcat [ postContext tags
                                      , constField "description" ""
                                      ]
            -- Учитываем 10 последних заметок...
            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
            renderRss myFeedConfiguration feedContext posts

    -- Обрабатываем главную страницу...
    match "index.html" $ do
        route idRoute
        compile $ do
            -- Показываем 7 последних заметок...
            posts <- fmap (take 7) . recentFirst =<< loadAll "posts/*"
            let indexContext = mconcat [ listField "posts" (postContext tags) (return posts) 
                                       , constField "title" "Мысли и опыт"
                                       , defaultContext
                                       ]

            getResourceBody >>= applyAsTemplate indexContext
                            >>= loadAndApplyTemplate "templates/default.html" indexContext
                            >>= relativizeUrls
    
    -- Создаём страницу для меток...
    create ["tags.html"] $ do
        route idRoute
        compile $ do
            let smallestFontSizeInPercent = 110
                biggestFontSizeInPercent = 330
                renderedCloud = \_ -> renderTagCloud smallestFontSizeInPercent 
                                                     biggestFontSizeInPercent
                                                     tags
                tagsContext = mconcat [ constField "title" "О чём беседуем"
                                      , field "tagsCloud" renderedCloud
                                      , defaultContext
                                      ]
            
            makeItem "" >>= loadAndApplyTemplate "templates/tags.html" tagsContext
                        >>= loadAndApplyTemplate "templates/default.html" tagsContext
                        >>= relativizeUrls

    -- Готовим все шаблоны из каталога templates...
    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

directorizeDate :: Routes
directorizeDate = customRoute (\i -> directorize $ toFilePath i)
    where
        directorize path = dirs 
            where
                (dirs, ext) = splitExtension $ 
                              concat $
                              (intersperse "/" date) ++ ["/"] ++ (intersperse "-" rest)
                (date, rest) = splitAt 3 $ splitOn "-" path

removePostsDirectoryFromURLs :: Routes
removePostsDirectoryFromURLs = gsubRoute "posts/" (const "")

postContext :: Tags -> Context String
postContext tags = mconcat [ constField "host" hostOfBlog
                           , dateField "date" "(%Y, %m, %d)" -- Дата будет выглядеть как (2014, 08, 27)
                           , tagsField "postTags" tags
                           , defaultContext
                           ]

