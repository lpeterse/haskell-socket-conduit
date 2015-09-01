{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Conduit
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--
-- This requests the Haskell website and prints it to stdout.
-- Note the use of IPv4-mapped `Inet6` addresses: This will work
-- even if you don't have IPv6 connectivity yet and is the preferred method
-- when writing new applications.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Conduit
-- >
-- > import Control.Concurrent.Async
-- > import Control.Monad
-- >
-- > import Data.ByteString.Lazy as B
-- > import Data.Monoid
-- >
-- > import System.Socket
-- > import System.Socket.Conduit (consumer, producer)
-- >
-- > main :: IO ()
-- > main = do
-- >   withConnectedSocket host port (aiAll `mappend` aiV4Mapped) $ \sock-> do
-- >     let _ = sock :: Socket Inet6 Stream TCP
-- >     void $ concurrently
-- >       (yield req $$ consumer sock mempty)
-- >       (producer sock buf mempty $$ stdoutC)
-- >   where
-- >     host = "www.haskell.org"
-- >     port = "80"
-- >     req  = "GET / HTTP/1.0\r\nHost: www.haskell.org\r\n\r\n"
-- >     buf  = 4096
--
-----------------------------------------------------------------------------

module System.Socket.Conduit where

import Control.Monad              (when)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Class  (lift)

import Data.Function              (fix)
import Data.Conduit               (Producer, Consumer, await, yield)
import Data.ByteString            (ByteString, null, length, drop)

import System.Socket              (Socket, MessageFlags, receive, send)

-- | Create a `Producer`.
--
--   - The second parameter is the read buffer size in bytes. @4096@ is a
--     sensible default.
--   - The socket is not closed automatically.
--   - Uses `System.Socket.receive` internally.
producer :: MonadIO m => Socket f t p -> Int -> MessageFlags -> Producer m ByteString
producer sock buf flags =
  fix $ \again-> do
    bs <- lift $ liftIO $ receive sock buf flags
    if Data.ByteString.null bs
      then return ()
      else yield bs >> again

-- | Create a 'Consumer'.
--
--   - The socket is not closed automatically.
--   - Uses `System.Socket.send` internally.
consumer :: MonadIO m => Socket f t p -> MessageFlags -> Consumer ByteString m ()
consumer sock flags =
  fix $ \again-> do
    mbs <- await
    case mbs of
      Just bs -> lift (liftIO $ sendAll' bs) >> again
      Nothing -> return ()
  where
    sendAll' bs = do
      sent <- send sock bs flags
      when (sent     < Data.ByteString.length bs)
           (sendAll' $ Data.ByteString.drop sent bs)
