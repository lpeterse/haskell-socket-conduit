{-# LANGUAGE RankNTypes #-}
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
