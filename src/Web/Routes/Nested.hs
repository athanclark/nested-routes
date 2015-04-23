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
  #-}

module Web.Routes.Nested
  ( module Web.Routes.Nested.FileExtListener
  , module Web.Routes.Nested.VerbListener
  , module Web.Routes.Nested.Types
  , HandlerT (..)
  , handleLit
  , handleParse
  , notFound
  , route
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


newtype HandlerT z m a = HandlerT
  { runHandler :: WriterT ( RUPTrie T.Text (Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ()))
                          , RUPTrie T.Text (Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ())) ) m a }
  deriving (Functor)

deriving instance Applicative m => Applicative (HandlerT z m)
deriving instance Monad m =>       Monad       (HandlerT z m)
deriving instance MonadIO m =>     MonadIO     (HandlerT z m)
instance MonadTrans (HandlerT z) where
  lift ma = HandlerT $ lift ma


handleLit :: ( Monad m
             , Singleton (UrlChunks xs)
                 (ExpectArity xs
                           (Either
                              (VerbListenerT z (FileExtListenerT Response m ()) m ())
                              (VerbListenerT z Response m ())))
                 (RUPTrie T.Text
                           (Either
                              (VerbListenerT z (FileExtListenerT Response m ()) m ())
                              (VerbListenerT z Response m ())))
             , Extrude (UrlChunks xs)
                 (RUPTrie T.Text
                           (Either
                              (VerbListenerT z (FileExtListenerT Response m ()) m ())
                              (VerbListenerT z Response m ())))
                 (RUPTrie T.Text
                           (Either
                              (VerbListenerT z (FileExtListenerT Response m ()) m ())
                              (VerbListenerT z Response m ())))
             ) =>
             UrlChunks xs
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
                    (P.merge child' $ singleton ts vl, mempty)

handleParse :: ( Monad m
               , Singleton (UrlChunks xs)
                   (ExpectArity xs
                           (Either
                              (VerbListenerT z (FileExtListenerT Response m ()) m ())
                              (VerbListenerT z Response m ())))
                   (RUPTrie T.Text
                           (Either
                              (VerbListenerT z (FileExtListenerT Response m ()) m ())
                              (VerbListenerT z Response m ())))
               , Extrude (UrlChunks xs)
                   (RUPTrie T.Text
                           (Either
                              (VerbListenerT z (FileExtListenerT Response m ()) m ())
                              (VerbListenerT z Response m ())))
                   (RUPTrie T.Text
                           (Either
                              (VerbListenerT z (FileExtListenerT Response m ()) m ())
                              (VerbListenerT z Response m ())))
               ) =>
               UrlChunks xs
            -> ExpectArity xs (Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ()))
            -> [HandlerT z m ()]
            -> HandlerT z m ()
handleParse ts vl [] =
  HandlerT $ tell (singleton ts vl, mempty)
handleParse ts vl cs = do
  (child,_) <- lift $ foldM (\acc c -> (acc <>) <$> (execWriterT $ runHandler c)) mempty cs

  HandlerT $ tell $ let
                      child' = extrude ts child
                    in
                    (P.merge child' $ singleton ts vl, mempty)


notFound :: ( Monad m
            , Singleton (UrlChunks xs)
                (ExpectArity xs
                           (Either
                              (VerbListenerT z (FileExtListenerT Response m ()) m ())
                              (VerbListenerT z Response m ())))
                (RUPTrie T.Text
                           (Either
                              (VerbListenerT z (FileExtListenerT Response m ()) m ())
                              (VerbListenerT z Response m ())))
            ) =>
            UrlChunks xs
         -> ExpectArity xs (Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ()))
         -> HandlerT z m ()
notFound ts vl = do
  HandlerT $ tell (mempty, singleton ts vl)


-- | Turns a @HandlerT@ into a Wai @Application@
route :: (Functor m, Monad m, MonadIO m) =>
         HandlerT z m a -- ^ Assembled @handle@ calls
      -> Request
      -> (Response -> IO ResponseReceived) -> m ResponseReceived
route h req respond = do
  (rtrie, nftrie) <- execWriterT $ runHandler h
  let mMethod  = httpMethodToMSym $ requestMethod req
      mFileext = case pathInfo req of
                         [] -> Just Html
                         xs -> possibleExts $ getFileExt $ last xs
      -- meitherNotFound :: Maybe (Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ()))
      meitherNotFound = P.lookupNearestParent (pathInfo req) nftrie

  notFoundBasic <- handleNotFound Html Get meitherNotFound

  case (mFileext, mMethod) of
    (Just f, Just v) -> do
      menf <- handleNotFound f v meitherNotFound
      let cleanedPathInfo = applyToLast trimFileExt $ pathInfo req
      case P.lookup cleanedPathInfo rtrie of
        Just eitherM -> continue f v eitherM $ menf
        Nothing  -> case pathInfo req of
          [] -> liftIO $ respond404 $ menf
          _  -> case trimFileExt $ last $ pathInfo req of
            "index" -> case P.lookup (init $ pathInfo req) rtrie of
              Just eitherM -> continue f v eitherM $ menf
              Nothing -> liftIO $ respond404 $ menf
            _       -> liftIO $ respond404 $ menf
    _ -> liftIO $ respond404 $ notFoundBasic

  where
    handleNotFound :: MonadIO m =>
                      FileExt
                   -> Verb
                   -> Maybe ( Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ()) )
                   -> m (Maybe Response)
    handleNotFound f v meitherNotFound = case meitherNotFound of
      Just (Left litmonad) -> do
        vmapLit <- execWriterT $ runVerbListenerT litmonad
        case M.lookup v (unVerbs vmapLit) of
          Just (_, femonad) -> do
            femap <- execWriterT $ runFileExtListenerT femonad
            return $ lookupMin f $ unFileExts femap
          Nothing -> return Nothing
      Just (Right predmonad) -> do
        vmapPred <- execWriterT $ runVerbListenerT predmonad
        case M.lookup v (unVerbs vmapPred) of
          Just (_, r) -> return $ Just r
          Nothing -> return Nothing
      Nothing -> return Nothing

    respond404 mr = respond $ fromMaybe plain404 mr

    continue :: MonadIO m =>
                FileExt
             -> Verb
             -> Either (VerbListenerT z (FileExtListenerT Response m ()) m ()) (VerbListenerT z Response m ())
             -> Maybe Response
             -> m ResponseReceived
    continue f v eitherM mnfResp = case eitherM of
       Left litmonad -> do
         vmapLit <- execWriterT $ runVerbListenerT litmonad
         continueLit f v (unVerbs vmapLit) mnfResp
       Right predmonad -> do
         vmapPred <- execWriterT $ runVerbListenerT predmonad
         continuePred f v (unVerbs vmapPred) mnfResp


    continueLit :: MonadIO m =>
                   FileExt
                -> Verb
                -> M.Map Verb (Maybe (ReaderT BL.ByteString m z, Maybe BodyLength), FileExtListenerT Response m ())
                -> Maybe Response
                -> m ResponseReceived
    continueLit f v vmap mnfResp = case M.lookup v vmap of
      Just (mreqbodyf, femonad) -> do
        femap <- execWriterT $ runFileExtListenerT femonad
        case lookupMin f $ unFileExts femap of
          Just r -> do
            case mreqbodyf of
              Nothing              -> liftIO $ respond r
              Just (reqbf,Nothing) -> do
                body <- liftIO $ strictRequestBody req
                (runReaderT $ reqbf) body
                liftIO $ respond r
              Just (reqbf,Just bl) -> do
                case requestBodyLength req of
                  KnownLength bl' -> if bl' <= bl
                                       then do body <- liftIO $ strictRequestBody req
                                               (runReaderT $ reqbf) body
                                               liftIO $ respond r
                                       else liftIO $ respond404 mnfResp
                  _ -> liftIO $ respond404 mnfResp
          Nothing -> liftIO $ respond404 mnfResp
      Nothing -> liftIO $ respond404 mnfResp


    continuePred :: MonadIO m =>
                    FileExt
                 -> Verb
                 -> M.Map Verb (Maybe (ReaderT BL.ByteString m z, Maybe BodyLength), Response)
                 -> Maybe Response
                 -> m ResponseReceived
    continuePred f v vmap mnfResp = case M.lookup v vmap of
        Just (mreqbodyf, r) ->
          case mreqbodyf of
            Nothing              -> liftIO $ respond r
            Just (reqbf,Nothing) -> do
              body <- liftIO $ strictRequestBody req
              (runReaderT $ reqbf) body
              liftIO $ respond r
            Just (reqbf,Just bl) -> do
              case requestBodyLength req of
                KnownLength bl' -> if bl' <= bl
                                     then do body <- liftIO $ strictRequestBody req
                                             (runReaderT $ reqbf) body
                                             liftIO $ respond r
                                     else liftIO $ respond404 mnfResp
                _ -> liftIO $ respond404 mnfResp
        Nothing -> liftIO $ respond404 mnfResp


    plain404 = responseLBS status404 [("Content-Type","text/plain")] "404"

    lookupMin :: Ord k => k -> M.Map k a -> Maybe a
    lookupMin k map | all (k <) (M.keys map) = M.lookup (minimum $ M.keys map) map
                    | otherwise              = M.lookup k map

    getFileExt :: T.Text -> T.Text
    getFileExt s =
      let mfound = foldl go Nothing $ T.unpack s in
      case mfound of
        Nothing -> T.pack ""
        Just x  -> T.pack x
      where
        go Nothing x | x == '.'  = Just "."
                     | otherwise = Nothing
        go (Just xs) x = Just $ xs ++ [x]

    applyToLast :: (a -> a) -> [a] -> [a]
    applyToLast f [] = []
    applyToLast f (x:[]) = f x : []
    applyToLast f (x:xs) = x : applyToLast f xs

    trimFileExt :: T.Text -> T.Text
    trimFileExt s = T.pack $ takeWhile (/= '.') $ T.unpack s

    httpMethodToMSym :: Method -> Maybe Verb
    httpMethodToMSym x | x == methodGet    = Just Get
                       | x == methodPost   = Just Post
                       | x == methodPut    = Just Put
                       | x == methodDelete = Just Delete
                       | otherwise         = Nothing
