{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}

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
import           Data.Trie.Pred.Unified
import qualified Data.Trie.Pred.Unified            as P
import           Data.Traversable
import qualified Data.Text                         as T
import qualified Data.Map.Lazy                     as M
import qualified Data.ByteString.Lazy              as BL
import           Data.Maybe                        (fromMaybe)
import           Unsafe.Coerce


newtype HandlerT z m a = HandlerT
  { runHandler :: WriterT ( RPUTrie T.Text (Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ()))
                          , RPUTrie T.Text (Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ())) ) m a }
  deriving (Functor)

deriving instance Applicative m => Applicative (HandlerT z m)
deriving instance Monad m =>       Monad       (HandlerT z m)
deriving instance MonadIO m =>     MonadIO     (HandlerT z m)
instance MonadTrans (HandlerT z) where
  lift ma = HandlerT $ lift ma


handleLit :: Monad m =>
             UrlChunks xs last
          -> ExpectArity xs (Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ()))
          -> [HandlerT z m ()]
          -> HandlerT z m ()
handleLit ts vl [] =
  HandlerT $ tell (singleton ts vl, mempty)
handleLit ts vl cs = do
  (child,_) <- lift $ foldM (\acc c -> (acc <>) <$> (execWriterT $ runHandler c)) mempty cs

  HandlerT $ tell $ let
                      child' = extrude ts child
                    in
                    (R.merge child' $ singleton ts vl, mempty)

handleParse :: Monad m =>
               UrlChunks xs 'PredLP
            -> ExpectArity xs (Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ()))
            -> [HandlerT z m ()]
            -> HandlerT z m ()
handleParse ts vl [] =
  HandlerT $ tell (singleton ts vl, mempty)
handleParse ts vl cs = do
  (child,_) <- lift $ foldM (\acc c -> (acc <>) <$> (execWriterT $ runHandler c)) mempty cs

  HandlerT $ tell $ let
                      -- child' :: ExpectArity xs (Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ()))
                      child' = extrude ts child
                    in
                    (R.merge child' $ singleton ts vl, mempty)


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
