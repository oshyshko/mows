module Mows.Logger
    ( info, withLogger
    ) where

import           Control.Exception.Safe (finally)
import           System.IO.Unsafe       (unsafePerformIO)
import           System.Log.FastLogger  (LoggerSet, defaultBufSize, flushLogStr,
                                         newStdoutLoggerSet, pushLogStrLn,
                                         toLogStr)

-- from https://eax.me/haskell-fast-logger/
{-# NOINLINE globalLogger #-}
globalLogger :: LoggerSet
globalLogger = unsafePerformIO $ newStdoutLoggerSet defaultBufSize

{-# INLINE info #-}
info :: String -> IO ()
info = pushLogStrLn globalLogger . toLogStr

withLogger :: IO a -> IO a
withLogger a = finally a $ flushLogStr globalLogger
