{-# LANGUAGE
    DeriveFunctor
  , DeriveTraversable
  , DeriveFoldable
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , TupleSections
  #-}

module Web.Routes.Nested.VerbListener where

import           Network.Wai (Request)
import           Network.HTTP.Types (StdMethod (..))

import           Data.Foldable
import           Data.Traversable
import           Data.Map                             as Map
import qualified Data.ByteString.Lazy                 as BL
import           Data.Word                            (Word64)
import           Control.Arrow
import           Control.Applicative hiding (empty)
import           Control.Monad.Trans
import           Control.Monad.Writer


type Verb = StdMethod

type BodyLength = Word64

newtype Verbs m r = Verbs
  { unVerbs :: Map Verb ( Maybe (BL.ByteString -> m (), Maybe BodyLength)
                        , Either r (Request -> r)
                        )
  } deriving (Monoid)

supplyReq :: Request
          -> Map Verb ( Maybe (BL.ByteString -> m (), Maybe BodyLength)
                      , Either r (Request -> r)
                      )
          -> Map Verb ( Maybe (BL.ByteString -> m (), Maybe BodyLength)
                      , r
                      )
supplyReq req xs = second (either id ($ req)) <$> xs

instance Functor (Verbs m) where
  fmap f (Verbs xs) = Verbs $ fmap go xs
    where go (x, Left r)  = (x, Left $ f r)
          go (x, Right r) = (x, Right $ f . r)

instance Foldable (Verbs m) where
  foldMap f (Verbs xs) = foldMap go xs
    where go (_, Left r) = f r
          go _ = mempty

newtype VerbListenerT r m a =
  VerbListenerT { runVerbListenerT :: WriterT (Verbs m r) m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (VerbListenerT r) where
  lift ma = VerbListenerT $ lift ma


foldMWithKey :: Monad m => (acc -> Verb -> a -> m acc) -> acc -> Map Verb a -> m acc
foldMWithKey f i = foldlWithKey (\macc k a -> (\mer -> f mer k a) =<< macc) (return i)


-- | For simple @GET@ responses
get :: ( Monad m
       ) => r -> VerbListenerT r m ()
get r = do
  let new = singleton GET (Nothing, Left r)
  VerbListenerT $ tell $ Verbs new

-- | Inspect the @Request@ object supplied by WAI
getReq :: ( Monad m
          ) => (Request -> r) -> VerbListenerT r m ()
getReq r = do
  let new = singleton GET (Nothing, Right r)
  VerbListenerT $ tell $ Verbs new


-- | For simple @POST@ responses
post :: ( Monad m
        , MonadIO m
        ) => (BL.ByteString -> m ()) -> r -> VerbListenerT r m ()
post handle r = do
  let new = singleton POST (Just (handle, Nothing), Left r)
  VerbListenerT $ tell $ Verbs new

-- | Inspect the @Request@ object supplied by WAI
postReq :: ( Monad m
           , MonadIO m
           ) => (BL.ByteString -> m ()) -> (Request -> r) -> VerbListenerT r m ()
postReq handle r = do
  let new = singleton POST (Just (handle, Nothing), Right r)
  VerbListenerT $ tell $ Verbs new

-- | Supply a maximum size bound for file uploads
postMax :: ( Monad m
           , MonadIO m
           ) => BodyLength -> (BL.ByteString -> m ()) -> r -> VerbListenerT r m ()
postMax bl handle r = do
  let new = singleton POST (Just (handle, Just bl), Left r)
  VerbListenerT $ tell $ Verbs new

-- | Inspect the @Request@ object supplied by WAI
postMaxReq :: ( Monad m
              , MonadIO m
              ) => BodyLength -> (BL.ByteString -> m ()) -> (Request -> r) -> VerbListenerT r m ()
postMaxReq bl handle r = do
  let new = singleton POST (Just (handle, Just bl), Right r)
  VerbListenerT $ tell $ Verbs new


-- | For simple @PUT@ responses
put :: ( Monad m
       , MonadIO m
       ) => (BL.ByteString -> m ()) -> r -> VerbListenerT r m ()
put handle r = do
  let new = singleton PUT (Just (handle, Nothing), Left r)
  VerbListenerT $ tell $ Verbs new

-- | Inspect the @Request@ object supplied by WAI
putReq :: ( Monad m
          , MonadIO m
          ) => (BL.ByteString -> m ()) -> (Request -> r) -> VerbListenerT r m ()
putReq handle r = do
  let new = singleton PUT (Just (handle, Nothing), Right r)
  VerbListenerT $ tell $ Verbs new

-- | Supply a maximum size bound for file uploads
putMax :: ( Monad m
          , MonadIO m
          ) => BodyLength -> (BL.ByteString -> m ()) -> r -> VerbListenerT r m ()
putMax bl handle r = do
  let new = singleton PUT (Just (handle, Just bl), Left r)
  VerbListenerT $ tell $ Verbs new

-- | Inspect the @Request@ object supplied by WAI
putMaxReq :: ( Monad m
             , MonadIO m
             ) => BodyLength -> (BL.ByteString -> m ()) -> (Request -> r) -> VerbListenerT r m ()
putMaxReq bl handle r = do
  let new = singleton PUT (Just (handle, Just bl), Right r)
  VerbListenerT $ tell $ Verbs new


-- | For simple @DELETE@ responses
delete :: ( Monad m
          ) => r -> VerbListenerT r m ()
delete r = do
  let new = singleton DELETE (Nothing, Left r)
  VerbListenerT $ tell $ Verbs new

-- | Inspect the @Request@ object supplied by WAI
deleteReq :: ( Monad m
             ) => (Request -> r) -> VerbListenerT r m ()
deleteReq r = do
  let new = singleton DELETE (Nothing, Right r)
  VerbListenerT $ tell $ Verbs new
