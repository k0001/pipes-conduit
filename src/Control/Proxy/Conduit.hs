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
  go (C.HaveOutput p c b) = P.respond b *> go p <* lift c -- this must be wrong
  go (C.NeedInput _ c)    = go (c ()) -- shouldn't this situation be an error?
  go (C.Done r)           = return r
  go (C.PipeM m)          = lift m >>= go
  go (C.Leftover p ())    = go p

