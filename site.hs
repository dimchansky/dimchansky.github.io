{-# LANGUAGE OverloadedStrings #-}

import           Crypto.Hash (Digest, MD5, hashlazy)
import           Control.Applicative
import           Data.Functor ((<$>))
import           Data.Binary (encode)
import           Data.List (isSuffixOf)
import           Data.Monoid (mappend)
import qualified Data.Set as S
import           Data.Time.Clock (nominalDiffTimeToSeconds)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           GHC.IO.Encoding (utf8, setLocaleEncoding, setFileSystemEncoding, setForeignEncoding)
import Hakyll
import           Text.Pandoc.Options
import           Text.Pandoc.Templates
import           Text.Pandoc.SideNote (usingSideNotes)
import           Text.Pandoc.Extensions (enableExtension)
import           Text.Regex (subRegex, mkRegex)
--------------------------------------------------------------------------------
main :: IO ()
main = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8
    runHakyll

runHakyll :: IO ()
runHakyll = hakyll $ do
    epoch <- preprocess $ nominalDiffTimeToSeconds <$> getPOSIXTime

    let timeHash = take 6 $ show (hashlazy (encode epoch) :: Digest MD5)
    let defaultCtxWithTimeHash =
            constField "timehash" timeHash <>
            defaultContext
    let siteCtx =
          deIndexedUrlField "url" `mappend`
          constField "root" (siteRoot siteConf) `mappend`
          constField "gaId" (siteGaId siteConf) `mappend`
          constField "feedUrl" "/atom.xml" `mappend`
          defaultCtxWithTimeHash
    let postCtx =
            deIndexedUrlField "url" `mappend`
            dateField "date" "%B %e, %Y" `mappend`
            dateField "datetime" "%Y-%m-%d" `mappend`
            siteCtx
    let tagsCtx tags =
            tagsField "prettytags" tags `mappend`
            postCtx
    let homeCtx tags list =
            constField "posts" list `mappend`
            constField "title" "Начало начал" `mappend`
            field "taglist" (\_ -> renderTagList tags) `mappend`
            siteCtx
    let allPostsCtx =
            constField "title" "Все заметки" `mappend`
            postCtx
    let postList tags pattern preprocess' = do
            postItemTpl <- loadBody "templates/postitem.html"
            posts <- loadAll pattern
            processed <- preprocess' posts
            applyTemplateList postItemTpl (tagsCtx tags) processed

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**.scss" $ do
        route   $ setExtension ".min.css"
        compile $ getResourceString >>=
            withItemBody (unixFilter "sass" ["--stdin", "--style=compressed"])

    match "posts/*" $ do
        route wordpressRoute
        compile $ do
            underlying <- getUnderlying
            toc <- getMetadataField underlying "toc"
            pandocCustomCompiler (maybe False (\bool -> bool == "true") toc)
                >>= applyFilter youtubeFilter
                >>= loadAndApplyTemplate "templates/post.html"        (tagsCtx tags)
                >>= loadAndApplyTemplate "templates/disqus.html"      (tagsCtx tags)
                >>= loadAndApplyTemplate "templates/default.html"     (tagsCtx tags)
                >>= relativizeUrls
                >>= deIndexUrls

    -- Render posts list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            sorted <- recentFirst posts
            itemTpl <- loadBody "templates/postitem.html"
            list <- applyTemplateList itemTpl postCtx sorted
            makeItem list
                >>= loadAndApplyTemplate "templates/posts.html" allPostsCtx
                >>= loadAndApplyTemplate "templates/default.html" allPostsCtx
                >>= relativizeUrls
                >>= deIndexUrls

    -- Index
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            sorted <- take 10 <$> recentFirst posts
            itemTpl <- loadBody "templates/postitem.html"
            list <- applyTemplateList itemTpl postCtx sorted
            makeItem list
                >>= loadAndApplyTemplate "templates/index.html" (homeCtx tags list)
                >>= loadAndApplyTemplate "templates/default.html" (homeCtx tags list)
                >>= relativizeUrls
                >>= deIndexUrls


    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Заметки помеченные “" ++ tag ++ "”"
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" title `mappend`
                         constField "body" list `mappend`
                         siteCtx)
                >>= loadAndApplyTemplate "templates/default.html"
                        (constField "title" title `mappend`
                         siteCtx)
                >>= relativizeUrls
                >>= deIndexUrls

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
            renderAtom myFeedConfiguration feedCtx posts

    create ["sitemap.xml"] $ do
        route   idRoute
        compile $ do
            posts <- loadAll "posts/*"
            let sitemapCtx =
                    listField "posts" postCtx (return posts) <>
                    siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

--------------------------------------------------------------------------------
-- Find and replace bare youtube links separated by <p></p>.
youtubeFilter :: String -> String
youtubeFilter x = subRegex regex x result
  where
    regex = mkRegex "<p>https?://www\\.youtube\\.com/watch\\?v=([A-Za-z0-9_-]+)</p>"
    result = "<div class=\"video-wrapper\">\
                \<div class=\"video-container\">\
                  \<iframe src=\"//www.youtube.com/embed/\\1\" frameborder=\"0\" allowfullscreen/>\
                \</div>\
             \</div>";
--------------------------------------------------------------------------------
applyFilter :: (Monad m, Functor f) => (String-> String) -> f String -> m (f String)
applyFilter transformator str = return $ (fmap $ transformator) str
--------------------------------------------------------------------------------
pandocCustomCompiler :: Bool -> Compiler (Item String)
pandocCustomCompiler withTOC = do
        tmpl <- either (const Nothing) Just <$> unsafeCompiler (
            compileTemplate
            "" $
            "\n$if(toc)$"
            <> "\n<nav id=\"toc\" role=\"doc-toc\">"
            <> "\n<strong>Содержание</strong>"
            <> "\n<label for=\"contents\">⊕</label>"
            <> "\n<input type=\"checkbox\" id=\"contents\">"
            <> "\n$toc$"
            <> "\n</nav>"
            <> "\n$endif$"
            <> "\n$body$"
            )

        let defaultExtensions = writerExtensions defaultHakyllWriterOptions
        let mathExtensions =
                [ Ext_tex_math_dollars
                , Ext_tex_math_double_backslash
                , Ext_latex_macros
                ]
        let newExtensions = foldr enableExtension defaultExtensions mathExtensions

        let writerOptions = defaultHakyllWriterOptions
                { writerExtensions = newExtensions
                , writerHTMLMathMethod = MathJax ""
                , writerTableOfContents = withTOC
                , writerTemplate = tmpl
                }

        pandocCompilerWithTransform
            defaultHakyllReaderOptions
            writerOptions
            usingSideNotes

--------------------------------------------------------------------------------
stripIndex :: String -> String
stripIndex url =
    if "index.html" `isSuffixOf` url && startsWithSlashOrDot url
    then take (length url - 10) url
    else url
  where
    startsWithSlashOrDot :: String -> Bool
    startsWithSlashOrDot (c:_) = c == '/' || c == '.'
    startsWithSlashOrDot []    = False

--------------------------------------------------------------------------------
deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndex) item

--------------------------------------------------------------------------------
deIndexedUrlField :: String -> Context a
deIndexedUrlField key = field key
  $ fmap (stripIndex . maybe empty toUrl) . getRoute . itemIdentifier


--------------------------------------------------------------------------------
wordpressRoute :: Routes
wordpressRoute =
    gsubRoute "^posts/[0-9]{4}-[0-9]{2}-[0-9]{2}-" (map replaceWithSlash) `composeRoutes`
    gsubRoute ".markdown$" (const "/index.html")
    where replaceWithSlash c = if c == '-' || c == '_'
                                   then '/'
                                   else c

--------------------------------------------------------------------------------
data SiteConfiguration = SiteConfiguration
    { siteRoot :: String
    , siteGaId :: String
    }

--------------------------------------------------------------------------------
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Непутёвые заметки Димчанского"
    , feedDescription = "Множество непутёвых заметок Димчанского"
    , feedAuthorName  = "Dmitrij Koniajev"
    , feedAuthorEmail = "dimchansky@gmail.com"
    , feedRoot        = "https://dimchansky.github.io"
    }

--------------------------------------------------------------------------------
siteConf :: SiteConfiguration
siteConf = SiteConfiguration
    { siteRoot = "https://dimchansky.github.io"
    , siteGaId = "G-B1YE0YL6FD"
    }
--------------------------------------------------------------------------------      