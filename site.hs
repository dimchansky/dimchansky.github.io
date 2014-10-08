--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Data.Functor ((<$>))
import           Data.List (isSuffixOf)
import           Data.Monoid (mappend)
import           Hakyll
import           GHC.IO.Encoding

--------------------------------------------------------------------------------
main :: IO ()
main = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8    
    runHakyll

runHakyll :: IO ()
runHakyll = hakyll $ do
    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= deIndexUrls

    match "posts/*" $ do
        route wordpressRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (tagsCtx tags)
            >>= loadAndApplyTemplate "templates/disqus.html"  (tagsCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (tagsCtx tags)
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
                         defaultContext)
                >>= loadAndApplyTemplate "templates/default.html"
                        (constField "title" title `mappend`
                         defaultContext)
                >>= relativizeUrls
				>>= deIndexUrls
                
    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description" 
            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
            renderAtom myFeedConfiguration feedCtx posts
			
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    deIndexedUrlField "url" `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    dateField "datetime" "%Y-%m-%d" `mappend`
    defaultContext

--------------------------------------------------------------------------------
tagsCtx :: Tags -> Context String
tagsCtx tags =
    tagsField "prettytags" tags `mappend`
    postCtx 

--------------------------------------------------------------------------------    
allPostsCtx :: Context String
allPostsCtx =
    constField "title" "Все заметки" `mappend`
    postCtx 

--------------------------------------------------------------------------------    
homeCtx :: Tags -> String -> Context String
homeCtx tags list =
    constField "posts" list `mappend`
    constField "title" "Начало начал" `mappend`
    field "taglist" (\_ -> renderTagList tags) `mappend`
    defaultContext

--------------------------------------------------------------------------------
postList :: Tags
         -> Pattern
         -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts <- loadAll pattern
    processed <- preprocess' posts
    applyTemplateList postItemTpl (tagsCtx tags) processed  

--------------------------------------------------------------------------------
stripIndex :: String -> String
stripIndex url = if "index.html" `isSuffixOf` url && elem (head url) "/."
  then take (length url - 10) url else url

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndex) item  
  
deIndexedUrlField :: String -> Context a
deIndexedUrlField key = field key
  $ fmap (stripIndex . maybe empty toUrl) . getRoute . itemIdentifier	
	
--------------------------------------------------------------------------------
wordpressRoute :: Routes
wordpressRoute =
    gsubRoute "posts/" (const "") `composeRoutes`
        gsubRoute "^[0-9]{4}-[0-9]{2}-[0-9]{2}-" (map replaceWithSlash)`composeRoutes`
            gsubRoute ".markdown" (const "/index.html")
    where replaceWithSlash c = if c == '-' || c == '_'
                                   then '/'
                                   else c   
    
--------------------------------------------------------------------------------    
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Непутёвые заметки Димчанского"
    , feedDescription = "Множество непутёвых заметок Димчанского"
    , feedAuthorName  = "Dmitrij Koniajev"
    , feedAuthorEmail = "dimchansky@gmail.com"
    , feedRoot        = "http://dimchansky.github.io"
    }   