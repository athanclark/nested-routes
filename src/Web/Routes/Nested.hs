{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}

module Web.Routes.Nested
  ( module Web.Routes.Nested.FileExtListener
  , module Web.Routes.Nested.VerbListener
  , HandlerT (..)
--  , handle
--  , notFound
--  , route
  ) where

import           Web.Routes.Nested.Types
import           Web.Routes.Nested.FileExtListener
import           Web.Routes.Nested.VerbListener

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Middleware.AddHeaders

import           Control.Applicative
import           Control.Arrow                     (second, first, (***))
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Control.Monad.Reader
import qualified Data.List.NonEmpty                as NE
import           Data.Monoid
import           Data.Trie.Pred
import qualified Data.Trie.Pred                    as P
import           Data.Trie.Pred.Rooted
import           Data.Trie.Pred.Rooted             as R
import           Data.Traversable
import qualified Data.Text                         as T
import qualified Data.Map.Lazy                     as M
import qualified Data.ByteString.Lazy              as BL
import           Data.Maybe                        (fromMaybe)


newtype HandlerT z m a = HandlerT
  { runHandler :: WriterT ( RootedPredTrie T.Text T.Text FileExts (Verbs z m Response)
                          , RootedPredTrie T.Text T.Text FileExts (Verbs z m Response) ) m a }
  deriving (Functor)

deriving instance Applicative m => Applicative (HandlerT z m)
deriving instance Monad m =>       Monad       (HandlerT z m)
deriving instance MonadIO m =>     MonadIO     (HandlerT z m)
instance MonadTrans (HandlerT z) where
  lift ma = HandlerT $ lift ma


-- | Add a path to the list of routes
-- handle :: Monad m =>
--           UrlChunks xs last                 -- ^ Input path, separated by slashes
--        -> ExpectArit xs (ExpectContents last)               -- ^ HTTP Method-oriented monad
--        -> [HandlerT z m ()]             -- ^ Child paths
--        -> HandlerT z m ()



-- handle ts vl [] = do
--   vfrs <- lift $ execWriterT $ runVerbListenerT vl
--
--   HandlerT $ tell $
--     case ts of
--       [] -> (MergeRooted $ Rooted (Just vfrs) [],                           mempty)
--       _  -> (MergeRooted $ Rooted Nothing     [Rest (NE.fromList ts) vfrs], mempty)
-- handle ts vl cs = do
--   vfrs <- lift $ execWriterT $ runVerbListenerT vl
--   (child,_) <- lift $ foldM (\acc c -> (acc <>) <$> (execWriterT $ runHandler c)) mempty cs
--
--   HandlerT $ tell $
--     case ts of
--       [] -> case unMergeRooted child of
--               Rooted _ xs -> (MergeRooted $ Rooted (Just vfrs) xs, mempty)
--       _  -> let child' = push (unMergeRooted child) $ NE.fromList ts in
--             (MergeRooted $ Rooted Nothing [P.assign (NE.fromList ts) (Just vfrs) child'], mempty)


-- notFound :: Monad m =>
--             [T.Text]
--          -> FileExtListenerT Response m ()
--          -> HandlerT z m ()
-- notFound ts flistener = do
--   (fes :: FileExts Response) <- lift $ execWriterT $ runFileExtListenerT flistener
--   HandlerT $ tell $
--     case ts of
--       [] -> (mempty, MergeRooted $ Rooted (Just fes) [])
--       _  -> (mempty, MergeRooted $ Rooted Nothing    [Rest (NE.fromList ts) fes])


-- | Turns a @HandlerT@ into a Wai @Application@
-- route :: (Functor m, Monad m, MonadIO m) =>
--          HandlerT z m a -- ^ Assembled @handle@ calls
--       -> Request
--       -> (Response -> IO ResponseReceived) -> m ResponseReceived
-- route h req respond = do
--   (rtrie, festrie) <- execWriterT $ runHandler h
--   let trie         = unMergeRooted rtrie
--       notFoundTrie = unMergeRooted festrie
--       mMethod      = httpMethodToMSym $ requestMethod req
--       mFileext     = case pathInfo req of
--                        [] -> Just Html
--                        xs -> possibleExts $ getFileExt $ last xs
--       mNotFoundMap :: Maybe (M.Map FileExt Response)
--       mNotFoundMap = (return . unFileExts) =<< R.lookupNearestParent (pathInfo req) notFoundTrie
--
--   case (mFileext, mMethod) of
--     (Just f, Just v) -> let cleanedPathInfo = applyToLast trimFileExt $ pathInfo req
--                         in
--                         case R.lookup cleanedPathInfo trie of
--       Just vmap -> continue f v vmap mNotFoundMap
--       Nothing  -> case trimFileExt $ last $ pathInfo req of
--         "index" -> case R.lookup (init $ pathInfo req) trie of
--           Just vmap -> continue f v vmap mNotFoundMap
--           Nothing -> liftIO $ respond $ fromMaybe plain404 $ mNotFoundMap >>= lookupMin f
--         _       -> liftIO $ respond $ fromMaybe plain404 $ mNotFoundMap >>= lookupMin f
--     _ -> liftIO $ respond $ fromMaybe plain404 $ mNotFoundMap >>= lookupMin Html
--
--   where
--     continue :: MonadIO m => FileExt -> Verb -> Verbs z m Response -> Maybe (M.Map FileExt Response) -> m ResponseReceived
--     continue f v vmap mNotFoundMap = case M.lookup v $ unVerbs vmap of
--         Just (mreqbodyf,femap) ->
--           case lookupMin f $ unFileExts femap of
--             Just r -> do
--               case mreqbodyf of
--                 Nothing    -> liftIO $ respond r
--                 Just (reqbf,Nothing) -> do
--                   body <- liftIO $ strictRequestBody req
--                   (runReaderT $ reqbf) body
--                   liftIO $ respond r
--                 Just (reqbf,Just bl) -> do
--                   case requestBodyLength req of
--                     KnownLength bl' -> if bl' <= bl
--                                          then do body <- liftIO $ strictRequestBody req
--                                                  (runReaderT $ reqbf) body
--                                                  liftIO $ respond r
--                                          else liftIO $ respond $ fromMaybe plain404 $ mNotFoundMap >>= lookupMin f
--                     _ -> liftIO $ respond $ fromMaybe plain404 $ mNotFoundMap >>= lookupMin f
--             Nothing -> liftIO $ respond $ fromMaybe plain404 $ mNotFoundMap >>= lookupMin f
--         Nothing -> liftIO $ respond $ fromMaybe plain404 $ lookupMin f =<< mNotFoundMap
--
--     plain404 = responseLBS status404 [("Content-Type","text/plain")] "404"
--
--     lookupMin :: Ord k => k -> M.Map k a -> Maybe a
--     lookupMin k map | all (k <) (M.keys map) = M.lookup (minimum $ M.keys map) map
--                     | otherwise              = M.lookup k map
--
--     getFileExt :: T.Text -> T.Text
--     getFileExt s =
--       let mfound = foldl go Nothing $ T.unpack s in
--       case mfound of
--         Nothing -> T.pack ""
--         Just x  -> T.pack x
--       where
--         go Nothing x | x == '.'  = Just "."
--                      | otherwise = Nothing
--         go (Just xs) x = Just $ xs ++ [x]
--
--     applyToLast :: (a -> a) -> [a] -> [a]
--     applyToLast f [] = []
--     applyToLast f (x:[]) = f x : []
--     applyToLast f (x:xs) = x : applyToLast f xs
--
--     trimFileExt :: T.Text -> T.Text
--     trimFileExt s = T.pack $ takeWhile (/= '.') $ T.unpack s
--
--     httpMethodToMSym :: Method -> Maybe Verb
--     httpMethodToMSym x | x == methodGet    = Just Get
--                        | x == methodPost   = Just Post
--                        | x == methodPut    = Just Put
--                        | x == methodDelete = Just Delete
--                        | otherwise         = Nothing
