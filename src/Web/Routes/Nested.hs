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
  , handle
  , route
  ) where

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
import           Data.Trie.Pseudo
import qualified Data.Trie.Pseudo                  as P
import           Data.Trie.Rooted
import qualified Data.Trie.Rooted                  as R
import           Data.Traversable
import qualified Data.Text                         as T
import qualified Data.Map.Lazy                     as M
import qualified Data.ByteString.Lazy              as BL


newtype HandlerT z m a = HandlerT
  { runHandler :: WriterT (MergeRooted T.Text (Verbs z m Response)) m a }
  deriving (Functor)

deriving instance Applicative m => Applicative (HandlerT z m)
deriving instance Monad m =>       Monad       (HandlerT z m)
deriving instance MonadIO m =>     MonadIO     (HandlerT z m)
instance MonadTrans (HandlerT z) where
  lift ma = HandlerT $ lift ma


-- | Add a path to the list of routes
handle :: Monad m =>
          [T.Text] -- ^ Input path, separated by slashes
       -> VerbListenerT z Response m () -- ^ HTTP Method-oriented monad
       -> [HandlerT z m ()] -- ^ Child paths
       -> HandlerT z m ()
handle ts vl [] = do
  vfrs <- lift $ execWriterT $ runVerbListenerT vl

  HandlerT $ tell $
    case ts of
      [] -> MergeRooted $ Rooted (Just vfrs) []
      _  -> MergeRooted $ Rooted Nothing [Rest (NE.fromList ts) vfrs]
handle ts vl cs = do
  vfrs <- lift $ execWriterT $ runVerbListenerT vl
  child <- lift $ foldM (\acc c -> (acc <>) <$> (execWriterT $ runHandler c)) mempty cs

  HandlerT $ tell $
    case ts of
      [] -> case unMergeRooted child of
              Rooted _ xs -> MergeRooted $ Rooted (Just vfrs) xs
      _  -> let child' = push (unMergeRooted child) $ NE.fromList ts in
            MergeRooted $ Rooted Nothing [P.assign (NE.fromList ts) (Just vfrs) child']


-- | Turns a @HandlerT@ into a Wai @Application@
route :: (Functor m, Monad m, MonadIO m) =>
         Response -- ^ Response to give when not found in the router
      -> HandlerT z m a -- ^ Assembled @handle@ calls
      -> Request
      -> (Response -> m b) -> m b
route notFound h req respond = do
  trie <- unMergeRooted <$> (execWriterT $ runHandler h)
  let mMethod = httpMethodToMSym $ requestMethod req
      mFileext = case pathInfo req of
                   [] -> Just Html
                   xs -> possibleExts $ getFileExt $ last xs

  case (mFileext, mMethod) of
    (Just f, Just v) -> let cleanedPathInfo = applyToLast trimFileExt $ pathInfo req in
                        case R.lookup cleanedPathInfo trie of
      Just vmap -> continue f v vmap
      Nothing  -> case trimFileExt $ last $ pathInfo req of
        "index" -> case R.lookup (init $ pathInfo req) trie of
          Just vmap -> continue f v vmap
          Nothing -> respond notFound
        _       -> respond notFound
    _ -> respond notFound

  where
    continue f v vmap = case M.lookup v $ unVerbs vmap of
        Just (mreqbodyf,femap) ->
          case lookupMin f $ unFileExts femap of
            Just r -> do
              case mreqbodyf of
                Nothing    -> respond r
                Just (reqbf,Nothing) -> do
                  body <- liftIO $ strictRequestBody req
                  (runReaderT $ reqbf) body
                  respond r
                Just (reqbf,Just bl) -> do
                  case requestBodyLength req of
                    KnownLength bl' -> if bl' <= bl
                                         then do body <- liftIO $ strictRequestBody req
                                                 (runReaderT $ reqbf) body
                                                 respond r
                                         else respond notFound
                    _ -> respond notFound
            Nothing -> respond notFound
        Nothing -> respond notFound

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
