module Mows.Prelude
    ( module Control.Concurrent
    , module Control.Concurrent.Async
    , module Control.Concurrent.STM
    , module Control.Concurrent.STM.TVar
    , module Control.Exception.Safe
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Data.Bool
    , module Data.ByteString
    , module Data.Function
    , module Data.Functor
    , module Data.IntMap.Strict
    , module Data.Map.Strict
    , module Data.Maybe
    , module Data.Set
    , module Data.Text
    , module Data.Vector.Unboxed
    , module Data.Vector.Unboxed.Mutable
    , module GHC.Generics
    , module System.Environment
    , module System.Random
    -- in-porject re-exports
    , module Mows.Logger
    , intToText
    ) where

import           Control.Concurrent          (forkIO, newEmptyMVar, putMVar,
                                              takeMVar, threadDelay)
import           Control.Concurrent.Async    (cancel, race_, withAsync)
import           Control.Concurrent.STM      (STM, atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar,
                                              newTVarIO, readTVar, readTVarIO,
                                              stateTVar, writeTVar)
import           Control.Exception.Safe      (Exception, SomeException, bracket,
                                              finally, handle, handleAny)
import           Control.Monad               (foldM, forM, forM_, forever,
                                              unless, when, msum)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Bool                   (bool)
import           Data.ByteString             (ByteString)
import           Data.Function               ((&))
import           Data.Functor                ((<&>))
import           Data.IntMap.Strict          (IntMap)
import           Data.Map.Strict             (Map)
import           Data.Maybe                  (fromMaybe, maybe)
import           Data.Set                    (Set)
import           Data.Text                   (Text)
import           Data.Vector.Unboxed         (Vector)
import           Data.Vector.Unboxed.Mutable (MVector)
import           GHC.Generics                (Generic)
import           System.Environment          (getArgs, lookupEnv)
import           System.Random               (randomIO)

import           Mows.Logger                 (info, withLogger)

-- not re-exported imports
import qualified Data.Text.Lazy              as T (toStrict)
import qualified Data.Text.Lazy.Builder      as T (toLazyText)
import qualified Data.Text.Lazy.Builder.Int  as T (decimal)

intToText :: Integral a => a -> Text
intToText = T.toStrict . T.toLazyText . T.decimal
