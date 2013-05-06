{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Proxy.Conduit where

import qualified Data.Conduit               as C
import qualified Data.Conduit.Internal      as C
import           Control.Applicative
import           Control.Monad.Trans.Class
import qualified Control.Proxy as P


-- DISCLAMER: I'm just starting with this! I'm not well aware of Conduit's
-- internals, so probably I'm doing many things wrong.

fromProducer :: (Monad m, P.Proxy p)
             => C.Producer m b
             -> () -> P.Producer p b m ()
fromProducer (C.ConduitM cp) () = P.runIdentityP (go cp) where
  go (C.HaveOutput p c b) = P.respond b *> go p <* lift c
  -- ^ the use of 'c' must be wrong here.
  go (C.NeedInput _ _)    = error "fromProducer: NeedInput"
  go (C.Done ())          = return ()
  go (C.PipeM m)          = lift m >>= go
  go (C.Leftover p ())    = go p


fromConsumer :: (Monad m, P.Proxy p)
             => C.Consumer a m r
             -> () -> P.Consumer p a m r
fromConsumer (C.ConduitM cp) () = P.runIdentityP (go cp) where
  go (C.HaveOutput _ _ _) = error "fromConsumer: HaveOutput"
  go (C.NeedInput p c)    = P.request () >>= go . p
  -- XXX what about 'c'?
  go (C.Done r)           = return r
  go (C.PipeM m)          = lift m >>= go
  go (C.Leftover p i)     = go p
  -- XXX what about 'i'?

