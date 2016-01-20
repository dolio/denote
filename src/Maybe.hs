{-# Language RankNTypes #-}

module Maybe where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict

test :: (forall m a b. Monad m => MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b)
     -> Maybe ()
test (<^>) = evalState (runMaybeT $ empty <^> undefined) 0

(<@>) :: Applicative m => MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
MaybeT mf <@> MaybeT mx = MaybeT $ h <$> mf <*> mx
 where h Nothing  _ = Nothing
       h (Just f) m = f <$> m
