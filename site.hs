--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mconcat)
import Data.List.Split (splitOn)
import Data.List (intersperse, isSuffixOf)
import System.FilePath (combine, splitExtension, takeFileName)
import Hakyll
--------------------------------------------------------------------------------

host :: String
host = "http://blog.dshevchenko.biz"

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration { feedTitle       = "Д. Шевченко"
                                        , feedDescription = "Мысли и опыт"
                                        , feedAuthorName  = "Денис Шевченко"
                                        , feedAuthorEmail = "me@dshevchenko.biz"
                                        , feedRoot        = host
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
    
    -- Просто копируем файл CNAME, нужный для поддержки 
    -- собственного dns на GitHub Pages...
    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    -- Создаём файл .nojekyll и просто копируем его.
    -- Он необходим для того, чтобы сообщить GitHub Pages
    -- о том, что этот сайт не на Jekyll...
    create [".nojekyll"] $ do
        route   idRoute
        compile copyFileCompiler

    -- Создаём .htaccess и применяем к нему специальный шаблон...
    create [".htaccess"] $ do
        route idRoute
        compile $ makeItem "" >>= loadAndApplyTemplate "templates/htaccess" defaultContext

    -- Строим теги из заметок...
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Обрабатываем все заметки...
    match "posts/*" $ do
        route $ removePostsDirectoryFromURLs `composeRoutes`
                directorizeDate `composeRoutes`
                setExtension "html"
        -- Используем pandocCompiler, потому что все заметки
        -- написаны на Markdown, и их необходимо превратить в html...
        compile $ pandocCompiler >>= loadAndApplyTemplate "templates/post.html" (postContext tags)
                                 >>= loadAndApplyTemplate "templates/default.html" (postContext tags)
                                 >>= relativizeUrls
    
    -- Создаём страницу 404 и применяем к ней шаблон стандартной страницы...
    create ["404.html"] $ do
        route idRoute
        compile $ makeItem "" >>= loadAndApplyTemplate "templates/404.html" (postContext tags)
                              >>= loadAndApplyTemplate "templates/default.html" (postContext tags)
                              >>= relativizeUrls

    -- Создаём страницу всех заметок...
    create ["archive.html"] $ do
        route idRoute
        compile $ do
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
                                         , constField "host" host
                                         , defaultContext
                                         ]

            makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapContext 

    --
--    tagsRules tags $ \tag pattern -> do
--        let title = "Posts tagged " ++ tag

--        route niceRoute
--        compile $ do
--          posts <- recentFirst =<< loadAll pattern
--          let ctx = constField "title" title <>
--                    listField "posts" (postCtx tags) (return posts) <>
--                    defaultContext

--          makeItem ""
--            >>= loadAndApplyTemplate "templates/tag.html" ctx
--            >>= loadAndApplyTemplate "templates/default.html" ctx
--            >>= relativizeUrls
--    create ["tags.html"] $ do
--        route idRoute
--        compile $ do
--            tags <- buildTags "posts/*" (fromCapture "tags/*.html")
--            renderTagCloud 12.0 36.0 tags
    
    -- Настраиваем RSS feed...
    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedContext = mconcat [ (postContext tags) 
                                      , constField "description" "This is the post description"
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
            let tagsContext = mconcat [ constField "title" "О чём беседуем"
                                      , field "tagsCloud" (\_ -> renderTagCloud 100 300 tags)
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
postContext tags = mconcat [ constField "host" host
                           , dateField "date" "(%Y, %m, %d)"
                           , tagsField "tags" tags
                           , defaultContext
                           ]

