module Handler.BlogPost where

import Import
import Yesod.Form.Bootstrap3 (bfs,renderBootstrap3, BootstrapFormLayout(..) )
import Yesod.Text.Markdown (markdownField)

blogPostForm :: Maybe BlogPost -> AForm Handler BlogPost
blogPostForm mBlogPost = BlogPost
          <$> areq textField (bfs MsgTitle) (blogPostTitle <$> mBlogPost)
          <*> areq markdownField (bfs MsgArticle) (blogPostArticle <$> mBlogPost)

getBlogPostNewR :: Handler Html
getBlogPostNewR = do
    uid <- requireAuthId
    (widget, encoding) <- generateFormPost $ 
                          renderBootstrap3 BootstrapBasicForm $ blogPostForm Nothing
    defaultLayout $ do
       $(widgetFile "blog/new")

postBlogPostNewR :: Handler Html
postBlogPostNewR = do
    ((result,widget), encoding) <- runFormPost $ 
                                    renderBootstrap3 BootstrapBasicForm  
                                    $ blogPostForm Nothing
    case result of
         FormSuccess blogPost -> do
                                 bId <- runDB $ insert blogPost
                                 redirect (BlogPostR bId)
         _ -> defaultLayout $ do
                    $(widgetFile "blog/new")

getBlogPostR :: BlogPostId -> Handler Html
getBlogPostR bpId = do
      blogPost <- runDB $ get404 bpId
      defaultLayout $ do
       $(widgetFile "blog/details")

deleteBlogPostR :: BlogPostId -> Handler Html
deleteBlogPostR bpId = do
     runDB $ delete bpId
     defaultLayout $ do
         [whamlet| |]

getBlogPostUpdateR :: BlogPostId -> Handler Html
getBlogPostUpdateR blogPostId = do
     blogPost <- runDB $ get404 blogPostId
     (widget, encoding) <- generateFormPost $
                          renderBootstrap3 BootstrapBasicForm $ blogPostForm (Just blogPost)
     defaultLayout $ do
       $(widgetFile "blog/edit")

postBlogPostUpdateR :: BlogPostId -> Handler Html
postBlogPostUpdateR blogPostId = do
    blogPost <- runDB $ get404 blogPostId
    ((result,widget), encoding) <- runFormPost $
                                    renderBootstrap3 BootstrapBasicForm 
                                    $ blogPostForm (Just blogPost)
    case result of
         FormSuccess blogPost -> do
                                 runDB $ replace blogPostId blogPost
                                 redirect (BlogPostR blogPostId)
         _ -> defaultLayout $ do
                    $(widgetFile "blog/edit")


