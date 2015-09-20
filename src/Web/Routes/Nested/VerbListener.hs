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
import           Data.Bifunctor
import           Data.Traversable
import           Data.Map (Map)
import qualified Data.Map                             as Map
import qualified Data.ByteString.Lazy                 as BL
import           Data.Word                            (Word64)
import           Data.Set.Class                       as Sets
import           Control.Arrow hiding (second)
import           Control.Applicative hiding (empty)
import           Control.Monad.Trans
import           Control.Monad.Writer


type Verb = StdMethod

type HandleUpload m u   = Request -> m (Maybe u)
type Respond u r        = Request -> Maybe u -> r
type ResponseSpec u m r = (HandleUpload m u, Respond u r)


-- * Verb Map

newtype Verbs u m r = Verbs
  { unVerbs :: Map Verb (ResponseSpec u m r)
  } deriving (Monoid, HasUnion, HasEmpty)

-- | To compensate for responses that want to peek into the @Request@ object.
supplyReq :: Request
          -> Map Verb (ResponseSpec u m r)
          -> Map Verb (m (Maybe u), Maybe u -> r)
supplyReq req xs = bimap ($ req) ($ req) <$> xs

instance Functor (Verbs u m) where
  fmap f (Verbs xs) = Verbs $ fmap (second (f .*)) xs

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
get r = tell $ Union $ Verbs $ Map.singleton GET ( const $ return Nothing
                                                 , const $ const r
                                                 )

-- | Inspect the @Request@ object supplied by WAI
getReq :: ( Monad m
          ) => (Request -> r) -> VerbListenerT r u m ()
getReq r = tell $ Union $ Verbs $ Map.singleton GET ( const $ return Nothing
                                                    , const . r)


-- | For simple @POST@ responses
post :: ( Monad m
        , MonadIO m
        ) => HandleUpload m u -> (Maybe u -> r) -> VerbListenerT r u m ()
post handle r = tell $ Union $ Verbs $ Map.singleton POST ( handle
                                                          , const r
                                                          )

-- | Inspect the @Request@ object supplied by WAI
postReq :: ( Monad m
           , MonadIO m
           ) => HandleUpload m u -> (Request -> Maybe u -> r) -> VerbListenerT r u m ()
postReq handle r = tell $ Union $ Verbs $ Map.singleton POST ( handle
                                                             , r
                                                             )


-- | For simple @PUT@ responses
put :: ( Monad m
       , MonadIO m
       ) => HandleUpload m u -> (Maybe u -> r) -> VerbListenerT r u m ()
put handle r = tell $ Union $ Verbs $ Map.singleton PUT ( handle
                                                        , const r
                                                        )

-- | Inspect the @Request@ object supplied by WAI
putReq :: ( Monad m
          , MonadIO m
          ) => HandleUpload m u -> (Request -> Maybe u -> r) -> VerbListenerT r u m ()
putReq handle r = tell $ Union $ Verbs $ Map.singleton PUT ( handle
                                                           , r
                                                           )


-- | For simple @DELETE@ responses
delete :: ( Monad m
          ) => r -> VerbListenerT r u m ()
delete r = tell $ Union $ Verbs $ Map.singleton DELETE ( const $ return Nothing
                                                       , const $ const r
                                                       )

-- | Inspect the @Request@ object supplied by WAI
deleteReq :: ( Monad m
             ) => (Request -> r) -> VerbListenerT r u m ()
deleteReq r = tell $ Union $ Verbs $ Map.singleton DELETE ( const $ return Nothing
                                                          , const . r
                                                          )
