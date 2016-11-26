{-# OPTIONS_GHC -Wall -i..  #-}
{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
-- originally from https://github.com/sanjoy/echoes/blob/master/src/Utils/Graph.hs
module Backends.D16Hoopl.GraphUtils(foldGraphNodes') where
import Prelude hiding ((<*>))
import Compiler.Hoopl

foldGraphNodes' :: forall n a .
                  (forall e x . n e x       -> a -> a)
               -> (forall e x . Graph n e x -> a -> a)

foldGraphNodes' f = graph
    where graph :: forall e x . Graph n e x -> a -> a
          lift  :: forall thing ex . (thing -> a -> a) -> (MaybeO ex thing -> a -> a)

          graph GNil              = id
          graph (GUnit b)         = block b
          graph (GMany e b x)     = lift block e . body b . lift block x
          body :: Body n -> a -> a
          body bdy                = \a -> mapFold block a bdy
          lift _ NothingO         = id
          lift fn (JustO thing)   = fn thing

          block :: Block n e x -> IndexedCO x a a -> IndexedCO e a a
          block = foldBlockNodesB f
