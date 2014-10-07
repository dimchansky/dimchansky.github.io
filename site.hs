--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Functor ((<$>))
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

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (tagsCtx tags)
			>>= loadAndApplyTemplate "templates/disqus.html"  (tagsCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (tagsCtx tags)
            >>= relativizeUrls

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
				
    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

tagsCtx :: Tags -> Context String
tagsCtx tags =
    tagsField "prettytags" tags `mappend`
    postCtx	
	
allPostsCtx :: Context String
allPostsCtx =
    constField "title" "Все заметки" `mappend`
    postCtx	
	
homeCtx :: Tags -> String -> Context String
homeCtx tags list =
    constField "posts" list `mappend`
    constField "title" "Index" `mappend`
    field "taglist" (\_ -> renderTagList tags) `mappend`
    defaultContext

postList :: Tags
         -> Pattern
         -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts <- loadAll pattern
    processed <- preprocess' posts
    applyTemplateList postItemTpl (tagsCtx tags) processed	