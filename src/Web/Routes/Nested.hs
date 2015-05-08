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
  , RankNTypes
  , PolyKinds
  , UndecidableInstances
  #-}

module Web.Routes.Nested
  ( module Web.Routes.Nested.FileExtListener
  , module Web.Routes.Nested.VerbListener
  , module Web.Routes.Nested.Types
  , HandlerT (..)
  , EitherResponse
  , handleLit
  , handleParse
  , notFoundLit
  , notFoundParse
  , route
  ) where

import           Web.Routes.Nested.Types
import           Web.Routes.Nested.FileExtListener
import           Web.Routes.Nested.VerbListener

import           Network.HTTP.Types
import           Network.HTTP.Media
import           Network.Wai

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Control.Monad.Reader
import           Data.Trie.Pred.Unified
import qualified Data.Trie.Pred.Unified            as P
import qualified Data.Text                         as T
import qualified Data.Map.Lazy                     as M
import qualified Data.ByteString.Lazy              as BL
import           Data.Maybe                        (fromMaybe)
import           Data.Constraint

import Data.Function.Poly


newtype HandlerT z x m a = HandlerT
  { runHandler :: WriterT ( RUPTrie T.Text x
                          , RUPTrie T.Text x ) m a }
  deriving (Functor)

deriving instance Applicative m => Applicative (HandlerT z x m)
deriving instance Monad m =>       Monad       (HandlerT z x m)
deriving instance MonadIO m =>     MonadIO     (HandlerT z x m)
instance MonadTrans (HandlerT z x) where
  lift ma = HandlerT $ lift ma


type EitherResponse z m = Either (VerbListenerT z (FileExtListenerT Response m ()) m ())
                                 (VerbListenerT z Response m ())

type family LastIsNothing (xs :: [Maybe *]) :: Constraint where
  LastIsNothing '[] = ()
  LastIsNothing ('Nothing ': '[]) = ()
  LastIsNothing (x ': xs) = LastIsNothing xs

type family LastIsJust (xs :: [Maybe *]) :: Constraint where
  LastIsJust (('Just x) ': '[]) = ()
  LastIsJust (x ': xs) = LastIsJust xs

-- | For routes ending with a literal.
handleLit :: ( Monad m
             , Functor m
             , cleanxs ~ OnlyJusts xs
             , HasResult childType (EitherResponse z m)
             , ExpectArity cleanxs childType
             , Singleton (UrlChunks xs)
                 childType
                 (RUPTrie T.Text result)
             , Extrude (UrlChunks xs)
                 (RUPTrie T.Text childType)
                 (RUPTrie T.Text result)
             , (ArityMinusTypeList childType cleanxs) ~ result
             , childType ~ TypeListToArity cleanxs result
             , LastIsNothing xs
             ) =>
             UrlChunks xs -- ^ Path to match against
          -> childType -- ^ Possibly a function, ending in @EitherResponse z m@
          -> Maybe (HandlerT z childType m ()) -- ^ Potential child routes
          -> HandlerT z result m ()
handleLit ts vl Nothing =
  HandlerT $ tell (singleton ts vl, mempty)
handleLit ts vl (Just cs) = do
  ((Rooted _ ctrie),_) <- lift $ execWriterT $ runHandler cs
  HandlerT $ tell (extrude ts $ Rooted (Just vl) ctrie, mempty)


-- | For routes ending with a parser.
handleParse :: ( Monad m
               , Functor m
               , cleanxs ~ OnlyJusts xs
               , HasResult childType (EitherResponse z m)
               , ExpectArity cleanxs childType
               , Singleton (UrlChunks xs)
                   childType
                   (RUPTrie T.Text result)
               , Extrude (UrlChunks xs)
                   (RUPTrie T.Text childType)
                   (RUPTrie T.Text result)
               , (ArityMinusTypeList childType cleanxs) ~ result
               , childType ~ TypeListToArity cleanxs result
               , LastIsJust xs
               ) =>
               UrlChunks xs
            -> childType
            -> Maybe (HandlerT z childType m ())
            -> HandlerT z result m ()
handleParse ts vl Nothing =
  HandlerT $ tell (singleton ts vl, mempty)
handleParse ts vl (Just cs) = do
  ((Rooted _ ctrie),_) <- lift $ execWriterT $ runHandler cs
  HandlerT $ tell (extrude ts $ Rooted (Just vl) ctrie, mempty)


notFoundLit :: ( Monad m
               , Functor m
               , cleanxs ~ OnlyJusts xs
               , HasResult childType (EitherResponse z m)
               , ExpectArity cleanxs childType
               , Singleton (UrlChunks xs)
                   childType
                   (RUPTrie T.Text result)
               , Extrude (UrlChunks xs)
                   (RUPTrie T.Text childType)
                   (RUPTrie T.Text result)
               , (ArityMinusTypeList childType cleanxs) ~ result
               , childType ~ TypeListToArity cleanxs result
               , LastIsNothing xs
               ) =>
               UrlChunks xs
            -> childType
            -> Maybe (HandlerT z childType m ())
            -> HandlerT z result m ()
notFoundLit ts vl Nothing = do
  HandlerT $ tell (mempty, singleton ts vl)
notFoundLit ts vl (Just cs) = do
  ((Rooted _ ctrie),_) <- lift $ execWriterT $ runHandler cs
  HandlerT $ tell (mempty, extrude ts $ Rooted (Just vl) ctrie)


notFoundParse :: ( Monad m
                 , Functor m
                 , cleanxs ~ OnlyJusts xs
                 , HasResult childType (EitherResponse z m)
                 , ExpectArity cleanxs childType
                 , Singleton (UrlChunks xs)
                     childType
                     (RUPTrie T.Text result)
                 , Extrude (UrlChunks xs)
                     (RUPTrie T.Text childType)
                     (RUPTrie T.Text result)
                 , (ArityMinusTypeList childType cleanxs) ~ result
                 , childType ~ TypeListToArity cleanxs result
                 , LastIsJust xs
                 ) =>
                 UrlChunks xs
              -> childType
              -> Maybe (HandlerT z childType m ())
              -> HandlerT z result m ()
notFoundParse ts vl Nothing = do
  HandlerT $ tell (mempty, singleton ts vl)
notFoundParse ts vl (Just cs) = do
  ((Rooted _ ctrie),_) <- lift $ execWriterT $ runHandler cs
  HandlerT $ tell (mempty, extrude ts $ Rooted (Just vl) ctrie)


-- | Turns a @HandlerT@ into a Wai @Application@
route :: ( Functor m
         , Monad m
         , MonadIO m
         ) =>
         HandlerT z (EitherResponse z m) m a -- ^ Assembled @handle@ calls
      -> Request
      -> (Response -> IO ResponseReceived) -> m ResponseReceived
route h req respond = do
  -- liftIO $ print $ (return . parseContentType) =<< (Prelude.lookup ("Accept" :: HeaderName) $ requestHeaders req)
  (rtrie, nftrie) <- execWriterT $ runHandler h
  let mMethod  = httpMethodToMSym $ requestMethod req
      mFileext = case pathInfo req of
                         [] -> Just Html
                         xs -> toExt $ T.pack $ dropWhile (/= '.') $ T.unpack $ last xs
      meitherNotFound = P.lookupNearestParent (pathInfo req) nftrie

  notFoundBasic <- handleNotFound (Just Html) Get meitherNotFound

  maybe (liftIO $ respond404 notFoundBasic) (\v -> do
    menf <- handleNotFound mFileext v meitherNotFound
    let cleanedPathInfo = applyToLast trimFileExt $ pathInfo req
        fail = liftIO $ respond404 menf

    maybe (case pathInfo req of
        [] -> fail
        _  -> case trimFileExt $ last $ pathInfo req of
          "index" -> maybe fail
                       (\eitherM -> continue mFileext v eitherM menf)
                       (P.lookup (init $ pathInfo req) rtrie)
          _ -> fail
      ) (\eitherM -> continue mFileext v eitherM menf)
      (P.lookup cleanedPathInfo rtrie)
    ) mMethod

  where
    onJustM :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
    onJustM f mx = maybe (return Nothing) f mx


    handleNotFound :: MonadIO m =>
                      Maybe FileExt
                   -> Verb
                   -> Maybe (EitherResponse z m)
                   -> m (Maybe Response)
    handleNotFound mf v meitherNotFound =
      let handleEither (Left litmonad) =
            onJustM (\f -> do
                vmapLit <- execWriterT $ runVerbListenerT litmonad
                onJustM (\(_, femonad) -> do
                    femap <- execWriterT $ runFileExtListenerT femonad
                    return $ lookupMin f $ unFileExts femap) $
                  M.lookup v $ unVerbs vmapLit) mf
          handleEither (Right predmonad) = do
            vmapPred <- execWriterT $ runVerbListenerT predmonad
            onJustM (\(_, r) -> return $ Just r) $ M.lookup v $ unVerbs vmapPred
      in
      onJustM handleEither meitherNotFound


    continue :: MonadIO m =>
                Maybe FileExt
             -> Verb
             -> EitherResponse z m
             -> Maybe Response
             -> m ResponseReceived
    continue mf v eitherM mnfResp = case eitherM of
       Left litmonad -> maybe (liftIO $ respond404 mnfResp) (\f -> do
                          vmapLit <- execWriterT $ runVerbListenerT litmonad
                          continueLit f v (unVerbs vmapLit) mnfResp)
                        mf
       Right predmonad -> do
         vmapPred <- execWriterT $ runVerbListenerT predmonad
         continuePred v (unVerbs vmapPred) mnfResp

    continueLit :: MonadIO m =>
                   FileExt
                -> Verb
                -> M.Map Verb (Maybe (ReaderT BL.ByteString m z, Maybe BodyLength), FileExtListenerT Response m ())
                -> Maybe Response
                -> m ResponseReceived
    continueLit f v vmap mnfResp =
      let fail = liftIO $ respond404 mnfResp in
      maybe fail (\(mreqbodyf, femonad) -> do
          femap <- execWriterT $ runFileExtListenerT femonad
          maybe fail (\r -> do
              case mreqbodyf of
                Nothing              -> liftIO $ respond r
                Just (reqbf,Nothing) -> do
                  body <- liftIO $ strictRequestBody req
                  (runReaderT $ reqbf) body
                  liftIO $ respond r
                Just (reqbf,Just bl) -> do
                  case requestBodyLength req of
                    KnownLength bl' ->
                      if bl' <= bl
                      then do body <- liftIO $ strictRequestBody req
                              (runReaderT $ reqbf) body
                              liftIO $ respond r
                      else fail
                    _ -> fail) $
            lookupMin f $ unFileExts femap) $ M.lookup v vmap


    continuePred :: MonadIO m =>
                    Verb
                 -> M.Map Verb (Maybe (ReaderT BL.ByteString m z, Maybe BodyLength), Response)
                 -> Maybe Response
                 -> m ResponseReceived
    continuePred v vmap mnfResp =
      let fail = liftIO $ respond404 mnfResp in
      maybe fail (\(mreqbodyf, r) ->
          case mreqbodyf of
            Nothing              -> liftIO $ respond r
            Just (reqbf,Nothing) -> do
              body <- liftIO $ strictRequestBody req
              (runReaderT $ reqbf) body
              liftIO $ respond r
            Just (reqbf,Just bl) -> do
              case requestBodyLength req of
                KnownLength bl' ->
                  if bl' <= bl
                  then do body <- liftIO $ strictRequestBody req
                          (runReaderT $ reqbf) body
                          liftIO $ respond r
                  else fail
                _ -> fail
          ) $ M.lookup v vmap


    respond404 :: Maybe Response -> IO ResponseReceived
    respond404 mr = respond $ fromMaybe plain404 mr

    plain404 :: Response
    plain404 = responseLBS status404 [("Content-Type","text/plain")] "404"

    lookupMin :: Ord k => k -> M.Map k a -> Maybe a
    lookupMin k map | all (k <) (M.keys map) = M.lookup (minimum $ M.keys map) map
                    | otherwise              = M.lookup k map

    lookupProper :: FileExt -> M.Map FileExt a -> Maybe a
    lookupProper k map = case M.lookup k map of
      Nothing -> case M.lookup (feSequence k !! 0) map of
        Nothing -> M.lookup (feSequence k !! 1) map
        Just x  -> Just x
      Just x  -> Just x
      where
        feSequence Html = [Text, Json]
        feSequence Json = [Text, Html]
        feSequence Text = [Json, Html]

    applyToLast :: (a -> a) -> [a] -> [a]
    applyToLast _ [] = []
    applyToLast f (x:[]) = f x : []
    applyToLast f (x:xs) = x : applyToLast f xs

    trimFileExt :: T.Text -> T.Text
    trimFileExt s = if (T.unpack s) `endsWithAny` possibleExts
                    then T.pack $ takeWhile (/= '.') $ T.unpack s
                    else s
      where
        possibleExts = [".html",".htm",".txt",".json"]
        endsWithAny s xs = (dropWhile (/= '.') s) `elem` xs

    httpMethodToMSym :: Method -> Maybe Verb
    httpMethodToMSym x | x == methodGet    = Just Get
                       | x == methodPost   = Just Post
                       | x == methodPut    = Just Put
                       | x == methodDelete = Just Delete
                       | otherwise         = Nothing
