{-# LANGUAGE
    DeriveFunctor
  , GADTs
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeOperators
  , OverloadedStrings
  , DataKinds
  , TupleSections
  , FlexibleContexts
  , ConstraintKinds
  , DataKinds
  , KindSignatures
  , TypeFamilies
  #-}

module Web.Routes.Nested
  ( module Web.Routes.Nested.FileExtListener
  , module Web.Routes.Nested.VerbListener
  , module Web.Routes.Nested.Types
  , HandlerT (..)
  , handleLit
  -- , handleParse
  -- , notFound
  -- , route
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
import           Data.Constraint

import Debug.Trace
import Data.Trie.Pred.Unified
import Data.Function.Poly
import Data.List (intercalate)


newtype HandlerT z x m a = HandlerT
  { runHandler :: WriterT ( RUPTrie T.Text x
                          , RUPTrie T.Text x ) m a }
  deriving (Functor)

deriving instance Applicative m => Applicative (HandlerT z x m)
deriving instance Monad m =>       Monad       (HandlerT z x m)
deriving instance MonadIO m =>     MonadIO     (HandlerT z x m)
instance MonadTrans (HandlerT z x) where
  lift ma = HandlerT $ lift ma


type family HeadIsNothing (xs :: [Maybe *]) :: Constraint where
  HeadIsNothing ('Nothing ': xs) = ()

type family HeadIsJust (xs :: [Maybe *]) :: Constraint where
  HeadIsJust (('Just x) ': xs) = ()

type family OnlyJusts (xs :: [Maybe *]) :: [*] where
  OnlyJusts '[] = '[]
  OnlyJusts ('Nothing  ': xs) = OnlyJusts xs
  OnlyJusts (('Just x) ': xs) = x ': OnlyJusts xs



handleLit :: ( Monad m
             , Functor m
             , cleanxs ~ OnlyJusts xs
             , HasResult singContent (Either
                                       (VerbListenerT z (FileExtListenerT Response m ()) m ())
                                       (VerbListenerT z Response m ()))
             , ExpectArity cleanxs singContent
             , Singleton (UrlChunks xs)
                 singContent
                 (RUPTrie T.Text (ArityMinusTypeList singContent cleanxs))
             , HasResult children (Either
                                    (VerbListenerT z (FileExtListenerT Response m ()) m ())
                                    (VerbListenerT z Response m ()))
             , ExpectArity cleanxs children
             , Extrude (UrlChunks xs)
                 (RUPTrie T.Text children)
                 (RUPTrie T.Text (ArityMinusTypeList children cleanxs))
             , (ArityMinusTypeList children cleanxs) ~ (ArityMinusTypeList singContent cleanxs)
             , (ArityMinusTypeList children cleanxs) ~ result
             , HeadIsNothing xs
             ) =>
             UrlChunks xs
          -> singContent
          -> [HandlerT z children m ()]
          -> HandlerT z result m ()
handleLit ts vl [] =
  HandlerT $ tell (singleton ts vl, mempty)
handleLit ts vl cs = do
  (child,_) <- lift $ foldM (\acc c -> (acc <>) <$> (execWriterT $ runHandler c)) mempty cs

  HandlerT $ tell $ let
                      child' = extrude ts child
                    in
                    (P.merge child' $ singleton ts vl, mempty)


handleParse :: ( Monad m
               , Functor m
               , cleanxs ~ OnlyJusts xs
               , HasResult singContent (Either
                                         (VerbListenerT z (FileExtListenerT Response m ()) m ())
                                         (VerbListenerT z Response m ()))
               , ExpectArity cleanxs singContent
               , Singleton (UrlChunks xs)
                   singContent
                   (RUPTrie T.Text (ArityMinusTypeList singContent cleanxs))
               , HasResult children (Either
                                      (VerbListenerT z (FileExtListenerT Response m ()) m ())
                                      (VerbListenerT z Response m ()))
               , ExpectArity cleanxs children
               , Extrude (UrlChunks xs)
                   (RUPTrie T.Text children)
                   (RUPTrie T.Text (ArityMinusTypeList children cleanxs))
               , (ArityMinusTypeList children cleanxs) ~ (ArityMinusTypeList singContent cleanxs)
               , (ArityMinusTypeList children cleanxs) ~ result
               , HeadIsJust xs
               ) =>
               UrlChunks xs
            -> singContent
            -> [HandlerT z children m ()]
            -> HandlerT z result m ()
handleParse ts vl [] =
  HandlerT $ tell (singleton ts vl, mempty)
handleParse ts vl cs = do
  (child,_) <- lift $ foldM (\acc c -> (acc <>) <$> (execWriterT $ runHandler c)) mempty cs

  HandlerT $ tell $ let
                      child' = extrude ts child
                    in
                    (P.merge child' $ singleton ts vl, mempty)


notFoundLit :: ( Monad m
               , Functor m
               , cleanxs ~ OnlyJusts xs
               , HasResult singContent (Either
                                         (VerbListenerT z (FileExtListenerT Response m ()) m ())
                                         (VerbListenerT z Response m ()))
               , ExpectArity cleanxs singContent
               , Singleton (UrlChunks xs)
                   singContent
                   (RUPTrie T.Text (ArityMinusTypeList singContent cleanxs))
               , HasResult children (Either
                                      (VerbListenerT z (FileExtListenerT Response m ()) m ())
                                      (VerbListenerT z Response m ()))
               , ExpectArity cleanxs children
               , Extrude (UrlChunks xs)
                   (RUPTrie T.Text children)
                   (RUPTrie T.Text (ArityMinusTypeList children cleanxs))
               , (ArityMinusTypeList children cleanxs) ~ (ArityMinusTypeList singContent cleanxs)
               , (ArityMinusTypeList children cleanxs) ~ result
               , HeadIsNothing xs
               ) =>
               UrlChunks xs
            -> singContent
            -> [HandlerT z children m ()]
            -> HandlerT z result m ()
notFoundLit ts vl [] = do
  HandlerT $ tell (mempty, singleton ts vl)
notFoundLit ts vl cs = do
  (child,_) <- lift $ foldM (\acc c -> (acc <>) <$> (execWriterT $ runHandler c)) mempty cs

  HandlerT $ tell $ let
                      child' = extrude ts child
                    in
                    (mempty, P.merge child' $ singleton ts vl)


notFoundParse :: ( Monad m
                 , Functor m
                 , cleanxs ~ OnlyJusts xs
                 , HasResult singContent (Either
                                           (VerbListenerT z (FileExtListenerT Response m ()) m ())
                                           (VerbListenerT z Response m ()))
                 , ExpectArity cleanxs singContent
                 , Singleton (UrlChunks xs)
                     singContent
                     (RUPTrie T.Text (ArityMinusTypeList singContent cleanxs))
                 , HasResult children (Either
                                        (VerbListenerT z (FileExtListenerT Response m ()) m ())
                                        (VerbListenerT z Response m ()))
                 , ExpectArity cleanxs children
                 , Extrude (UrlChunks xs)
                     (RUPTrie T.Text children)
                     (RUPTrie T.Text (ArityMinusTypeList children cleanxs))
                 , (ArityMinusTypeList children cleanxs) ~ (ArityMinusTypeList singContent cleanxs)
                 , (ArityMinusTypeList children cleanxs) ~ result
                 , HeadIsJust xs
                 ) =>
                 UrlChunks xs
              -> singContent
              -> [HandlerT z children m ()]
              -> HandlerT z result m ()
notFoundParse ts vl [] = do
  HandlerT $ tell (mempty, singleton ts vl)
notFoundParse ts vl cs = do
  (child,_) <- lift $ foldM (\acc c -> (acc <>) <$> (execWriterT $ runHandler c)) mempty cs

  HandlerT $ tell $ let
                      child' = extrude ts child
                    in
                    (mempty, P.merge child' $ singleton ts vl)


-- | Turns a @HandlerT@ into a Wai @Application@
-- route :: (Functor m, Monad m, MonadIO m) =>
--          HandlerT z m a -- ^ Assembled @handle@ calls
--       -> Request
--       -> (Response -> IO ResponseReceived) -> m ResponseReceived
-- route h req respond = do
--   (rtrie, nftrie) <- execWriterT $ runHandler h
--   let mMethod  = httpMethodToMSym $ requestMethod req
--       mFileext = case pathInfo req of
--                          [] -> Just Html
--                          xs -> toExt $ T.pack $ dropWhile (/= '.') $ T.unpack $ last xs
--       meitherNotFound = P.lookupNearestParent (pathInfo req) nftrie
--
--   notFoundBasic <- handleNotFound (Just Html) Get meitherNotFound
--
--   case mMethod of
--     Just v -> do
--       menf <- handleNotFound mFileext v meitherNotFound
--       let cleanedPathInfo = applyToLast trimFileExt $ pathInfo req
--       case P.lookup cleanedPathInfo rtrie of
--         Just eitherM -> continue mFileext v eitherM $ menf
--         Nothing  -> case pathInfo req of
--           [] -> liftIO $ respond404 $ menf
--           _  -> case trimFileExt $ last $ pathInfo req of
--             "index" -> case P.lookup (init $ pathInfo req) rtrie of
--               Just eitherM -> continue mFileext v eitherM $ menf
--               Nothing -> liftIO $ respond404 $ menf
--             _ -> liftIO $ respond404 $ menf
--     _ -> liftIO $ respond404 $ notFoundBasic
--
--   where
--     handleNotFound :: MonadIO m =>
--                       Maybe FileExt
--                    -> Verb
--                    -> Maybe ( Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ()) )
--                    -> m (Maybe Response)
--     handleNotFound mf v meitherNotFound = case meitherNotFound of
--       Just (Left litmonad) -> case mf of
--         Nothing -> return Nothing
--         Just f -> do
--           vmapLit <- execWriterT $ runVerbListenerT litmonad
--           case M.lookup v (unVerbs vmapLit) of
--             Just (_, femonad) -> do
--               femap <- execWriterT $ runFileExtListenerT femonad
--               return $ lookupMin f $ unFileExts femap
--             Nothing -> return Nothing
--       Just (Right predmonad) -> do
--         vmapPred <- execWriterT $ runVerbListenerT predmonad
--         case M.lookup v (unVerbs vmapPred) of
--           Just (_, r) -> return $ Just r
--           Nothing -> return Nothing
--       Nothing -> return Nothing
--
--
--     continue :: MonadIO m =>
--                 Maybe FileExt
--              -> Verb
--              -> Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ())
--              -> Maybe Response
--              -> m ResponseReceived
--     continue mf v eitherM mnfResp = case eitherM of
--        Left litmonad -> case mf of
--          Nothing -> liftIO $ respond404 mnfResp -- file extension parse failed
--          Just f -> do
--            vmapLit <- execWriterT $ runVerbListenerT litmonad
--            continueLit f v (unVerbs vmapLit) mnfResp
--        Right predmonad -> do
--          vmapPred <- execWriterT $ runVerbListenerT predmonad
--          continuePred v (unVerbs vmapPred) mnfResp
--
--
--     continueLit :: MonadIO m =>
--                    FileExt
--                 -> Verb
--                 -> M.Map Verb (Maybe (ReaderT BL.ByteString m z, Maybe BodyLength), FileExtListenerT Response m ())
--                 -> Maybe Response
--                 -> m ResponseReceived
--     continueLit f v vmap mnfResp = case M.lookup v vmap of
--       Just (mreqbodyf, femonad) -> do
--         femap <- execWriterT $ runFileExtListenerT femonad
--         case lookupMin f $ unFileExts femap of
--           Just r -> do
--             case mreqbodyf of
--               Nothing              -> liftIO $ respond r
--               Just (reqbf,Nothing) -> do
--                 body <- liftIO $ strictRequestBody req
--                 (runReaderT $ reqbf) body
--                 liftIO $ respond r
--               Just (reqbf,Just bl) -> do
--                 case requestBodyLength req of
--                   KnownLength bl' -> if bl' <= bl
--                                        then do body <- liftIO $ strictRequestBody req
--                                                (runReaderT $ reqbf) body
--                                                liftIO $ respond r
--                                        else liftIO $ respond404 mnfResp
--                   _ -> liftIO $ respond404 mnfResp
--           Nothing -> liftIO $ respond404 mnfResp
--       Nothing -> liftIO $ respond404 mnfResp
--
--
--     continuePred :: MonadIO m =>
--                     Verb
--                  -> M.Map Verb (Maybe (ReaderT BL.ByteString m z, Maybe BodyLength), Response)
--                  -> Maybe Response
--                  -> m ResponseReceived
--     continuePred v vmap mnfResp = case M.lookup v vmap of
--         Just (mreqbodyf, r) ->
--           case mreqbodyf of
--             Nothing              -> liftIO $ respond r
--             Just (reqbf,Nothing) -> do
--               body <- liftIO $ strictRequestBody req
--               (runReaderT $ reqbf) body
--               liftIO $ respond r
--             Just (reqbf,Just bl) -> do
--               case requestBodyLength req of
--                 KnownLength bl' -> if bl' <= bl
--                                      then do body <- liftIO $ strictRequestBody req
--                                              (runReaderT $ reqbf) body
--                                              liftIO $ respond r
--                                      else liftIO $ respond404 mnfResp
--                 _ -> liftIO $ respond404 mnfResp
--         Nothing -> liftIO $ respond404 mnfResp
--
--
--     respond404 mr = respond $ fromMaybe plain404 mr
--     plain404 = responseLBS status404 [("Content-Type","text/plain")] "404"
--
--     lookupMin :: Ord k => k -> M.Map k a -> Maybe a
--     lookupMin k map | all (k <) (M.keys map) = M.lookup (minimum $ M.keys map) map
--                     | otherwise              = M.lookup k map
--
--     applyToLast :: (a -> a) -> [a] -> [a]
--     applyToLast f [] = []
--     applyToLast f (x:[]) = f x : []
--     applyToLast f (x:xs) = x : applyToLast f xs
--
--     trimFileExt :: T.Text -> T.Text
--     trimFileExt s = if (T.unpack s) `endsWithAny` possibleExts
--                     then T.pack $ takeWhile (/= '.') $ T.unpack s
--                     else s
--       where
--         possibleExts = [".html",".htm",".txt",".json"]
--         endsWithAny s xs = (dropWhile (/= '.') s) `elem` xs
--
--     httpMethodToMSym :: Method -> Maybe Verb
--     httpMethodToMSym x | x == methodGet    = Just Get
--                        | x == methodPost   = Just Post
--                        | x == methodPut    = Just Put
--                        | x == methodDelete = Just Delete
--                        | otherwise         = Nothing
