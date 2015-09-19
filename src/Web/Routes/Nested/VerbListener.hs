{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , TupleSections
  #-}

module Web.Routes.Nested.VerbListener where

import           Network.Wai (Request)
import           Network.HTTP.Types (StdMethod (..))

import           Data.Foldable
import           Data.Function.Syntax
import           Data.Traversable
import           Data.Map (Map)
import qualified Data.Map                             as Map
import qualified Data.ByteString.Lazy                 as BL
import           Data.Word                            (Word64)
import           Data.Set.Class                       as Sets
import           Control.Arrow
import           Control.Applicative hiding (empty)
import           Control.Monad.Trans
import           Control.Monad.Writer


type Verb = StdMethod

type BodyLength = Word64
type Upload u m = BL.ByteString -> m (Maybe u)
type HandleUpload u m = Maybe (Upload u m, Maybe BodyLength)

type RespondEither r u = Either (Maybe u -> r) (Request -> Maybe u -> r)
type ResponseSpec r u m = (HandleUpload u m, RespondEither r u)


-- * Verb Map

newtype Verbs u m r = Verbs
  { unVerbs :: Map Verb (ResponseSpec r u m)
  } deriving (Monoid, HasUnion, HasEmpty)

-- | To compensate for responses that want to peek into the @Request@ object.
supplyReq :: Request
          -> Map Verb (ResponseSpec r u m)
          -> Map Verb (HandleUpload u m, Maybe u -> r)
supplyReq req xs = second (either id ($ req)) <$> xs

instance Functor (Verbs u m) where
  fmap f (Verbs xs) = Verbs $ fmap go xs
    where go (x, Left r)  = (x, Left $ f . r)
          go (x, Right r) = (x, Right $ f .* r)

-- instance Foldable (Verbs u m) where
--   foldMap f (Verbs xs) = foldMap go xs
--     where go (_, Left r) = f r -- can only take left cases
--           go _ = mempty


-- * Verb Writer

newtype VerbListenerT r u m a =
  VerbListenerT { runVerbListenerT :: WriterT (Union (Verbs u m r)) m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadWriter (Union (Verbs u m r))
             , MonadIO
             )

execVerbListenerT :: Monad m => VerbListenerT r u m a -> m (Verbs u m r)
execVerbListenerT verbs = do uVerbs <- execWriterT $ runVerbListenerT verbs
                             return $ unUnion uVerbs

instance MonadTrans (VerbListenerT r u) where
  lift ma = VerbListenerT $ lift ma


foldMWithKey :: Monad m => (acc -> Verb -> a -> m acc) -> acc -> Map Verb a -> m acc
foldMWithKey f i = Map.foldlWithKey (\macc k a -> (\mer -> f mer k a) =<< macc) (return i)


-- | For simple @GET@ responses
get :: ( Monad m
       ) => r -> VerbListenerT r u m ()
get r = tell $ Union $ Verbs $ Map.singleton GET (Nothing, Left $ const r)

-- | Inspect the @Request@ object supplied by WAI
getReq :: ( Monad m
          ) => (Request -> r) -> VerbListenerT r u m ()
getReq r = tell $ Union $ Verbs $ Map.singleton GET (Nothing, Right $ const . r)


-- | For simple @POST@ responses
post :: ( Monad m
        , MonadIO m
        ) => Upload u m -> (Maybe u -> r) -> VerbListenerT r u m ()
post handle r = tell $ Union $ Verbs $ Map.singleton POST (Just (handle, Nothing), Left r)

-- | Inspect the @Request@ object supplied by WAI
postReq :: ( Monad m
           , MonadIO m
           ) => Upload u m -> (Request -> Maybe u -> r) -> VerbListenerT r u m ()
postReq handle r = tell $ Union $ Verbs $ Map.singleton POST (Just (handle, Nothing), Right r)

-- | Supply a maximum size bound for file uploads
postMax :: ( Monad m
           , MonadIO m
           ) => BodyLength -> Upload u m -> (Maybe u -> r) -> VerbListenerT r u m ()
postMax bl handle r = tell $ Union $ Verbs $ Map.singleton POST (Just (handle, Just bl), Left r)

-- | Inspect the @Request@ object supplied by WAI
postMaxReq :: ( Monad m
              , MonadIO m
              ) => BodyLength -> Upload u m -> (Request -> Maybe u -> r) -> VerbListenerT r u m ()
postMaxReq bl handle r = tell $ Union $ Verbs $ Map.singleton POST (Just (handle, Just bl), Right r)


-- | For simple @PUT@ responses
put :: ( Monad m
       , MonadIO m
       ) => Upload u m -> r -> VerbListenerT r u m ()
put handle r = tell $ Union $ Verbs $ Map.singleton PUT (Just (handle, Nothing), Left $ const r)

-- | Inspect the @Request@ object supplied by WAI
putReq :: ( Monad m
          , MonadIO m
          ) => Upload u m -> (Request -> r) -> VerbListenerT r u m ()
putReq handle r = tell $ Union $ Verbs $ Map.singleton PUT (Just (handle, Nothing), Right $ const . r)

-- | Supply a maximum size bound for file uploads
putMax :: ( Monad m
          , MonadIO m
          ) => BodyLength -> Upload u m -> (Maybe u -> r) -> VerbListenerT r u m ()
putMax bl handle r = tell $ Union $ Verbs $ Map.singleton PUT (Just (handle, Just bl), Left r)

-- | Inspect the @Request@ object supplied by WAI
putMaxReq :: ( Monad m
             , MonadIO m
             ) => BodyLength -> Upload u m -> (Request -> Maybe u -> r) -> VerbListenerT r u m ()
putMaxReq bl handle r = tell $ Union $ Verbs $ Map.singleton PUT (Just (handle, Just bl), Right r)


-- | For simple @DELETE@ responses
delete :: ( Monad m
          ) => r -> VerbListenerT r u m ()
delete r = tell $ Union $ Verbs $ Map.singleton DELETE (Nothing, Left $ const r)

-- | Inspect the @Request@ object supplied by WAI
deleteReq :: ( Monad m
             ) => (Request -> r) -> VerbListenerT r u m ()
deleteReq r = tell $ Union $ Verbs $ Map.singleton DELETE (Nothing, Right $ const . r)
