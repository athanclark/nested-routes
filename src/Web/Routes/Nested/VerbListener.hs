{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , TupleSections
  #-}

module Web.Routes.Nested.VerbListener where

import           Network.Wai (Request)
import           Network.HTTP.Types (StdMethod (..))

import           Data.Function.Syntax
import           Data.Bifunctor
import           Data.Map (Map)
import qualified Data.Map                             as Map
import           Data.Set.Class                       as Sets
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Control.Monad.Except


type Verb = StdMethod

type HandleUpload e m u   = Request -> ExceptT (Maybe e) m u
type Respond e u r        = Request -> Either (Maybe e) u -> r
type ResponseSpec e u m r = (HandleUpload e m u, Respond e u r)


-- * Verb Map

newtype Verbs e u m r = Verbs
  { unVerbs :: Map Verb (ResponseSpec e u m r)
  } deriving (Monoid, HasUnion, HasEmpty)

-- | To compensate for responses that want to peek into the @Request@ object.
supplyReq :: Request
          -> Map Verb (ResponseSpec e u m r)
          -> Map Verb (ExceptT (Maybe e) m u, Either (Maybe e) u -> r)
supplyReq req xs = bimap ($ req) ($ req) <$> xs

instance Functor (Verbs e u m) where
  fmap f (Verbs xs) = Verbs $ fmap (second (f .*)) xs

-- instance Foldable (Verbs u m) where
--   foldMap f (Verbs xs) = foldMap go xs
--     where go (_, Left r) = f r -- can only take left cases
--           go _ = mempty


-- * Verb Writer

newtype VerbListenerT r e u m a =
  VerbListenerT { runVerbListenerT :: WriterT (Union (Verbs e u m r)) m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadWriter (Union (Verbs e u m r))
             , MonadIO
             )

execVerbListenerT :: Monad m => VerbListenerT r e u m a -> m (Verbs e u m r)
execVerbListenerT verbs = do uVerbs <- execWriterT $ runVerbListenerT verbs
                             return $ unUnion uVerbs

instance MonadTrans (VerbListenerT r e u) where
  lift ma = VerbListenerT $ lift ma


foldMWithKey :: Monad m => (acc -> Verb -> a -> m acc) -> acc -> Map Verb a -> m acc
foldMWithKey f i = Map.foldlWithKey (\macc k a -> (\mer -> f mer k a) =<< macc) (return i)


-- | For simple @GET@ responses
get :: ( Monad m
       ) => r -> VerbListenerT r e u m ()
get r = tell $ Union $ Verbs $ Map.singleton GET ( const $ throwError Nothing
                                                 , const $ const r
                                                 )

-- | Inspect the @Request@ object supplied by WAI
getReq :: ( Monad m
          ) => (Request -> r) -> VerbListenerT r e u m ()
getReq r = tell $ Union $ Verbs $ Map.singleton GET ( const $ throwError Nothing
                                                    , const . r)


-- | For simple @POST@ responses
post :: ( Monad m
        , MonadIO m
        ) => HandleUpload e m u -> (Either (Maybe e) u -> r) -> VerbListenerT r e u m ()
post handle r = tell $ Union $ Verbs $ Map.singleton POST ( handle
                                                          , const r
                                                          )

-- | Inspect the @Request@ object supplied by WAI
postReq :: ( Monad m
           , MonadIO m
           ) => HandleUpload e m u -> Respond e u r -> VerbListenerT r e u m ()
postReq handle r = tell $ Union $ Verbs $ Map.singleton POST ( handle
                                                             , r
                                                             )


-- | For simple @PUT@ responses
put :: ( Monad m
       , MonadIO m
       ) => HandleUpload e m u -> (Either (Maybe e) u -> r) -> VerbListenerT r e u m ()
put handle r = tell $ Union $ Verbs $ Map.singleton PUT ( handle
                                                        , const r
                                                        )

-- | Inspect the @Request@ object supplied by WAI
putReq :: ( Monad m
          , MonadIO m
          ) => HandleUpload e m u -> Respond e u r -> VerbListenerT r e u m ()
putReq handle r = tell $ Union $ Verbs $ Map.singleton PUT ( handle
                                                           , r
                                                           )


-- | For simple @DELETE@ responses
delete :: ( Monad m
          ) => r -> VerbListenerT r e u m ()
delete r = tell $ Union $ Verbs $ Map.singleton DELETE ( const $ throwError Nothing
                                                       , const $ const r
                                                       )

-- | Inspect the @Request@ object supplied by WAI
deleteReq :: ( Monad m
             ) => (Request -> r) -> VerbListenerT r e u m ()
deleteReq r = tell $ Union $ Verbs $ Map.singleton DELETE ( const $ throwError Nothing
                                                          , const . r
                                                          )
