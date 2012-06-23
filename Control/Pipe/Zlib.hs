{-# LANGUAGE FlexibleContexts #-}
module Control.Pipe.Zlib (
  gzip,
  gunzip,
  decompress,
  compress
  ) where

import Codec.Zlib
import Control.Monad
import Control.Monad.IO.Class
import Control.Pipe
import Control.Pipe.Class
import qualified Data.ByteString as B
import Prelude hiding (catch)

-- | Gzip compression with default parameters.
gzip :: (MonadStream m, MonadIO (BaseMonad m)) => m B.ByteString B.ByteString r r
gzip = compress 1 (WindowBits 31)

-- | Gzip decompression with default parameters.
gunzip :: (MonadStream m, MonadIO (BaseMonad m)) => m B.ByteString B.ByteString r r
gunzip = decompress (WindowBits 31)

decompress
    :: (MonadStream m, MonadIO (BaseMonad m))
    => WindowBits
    -> m B.ByteString B.ByteString r r
decompress config = do
    inf <- liftIO $ initInflate config
    r <- forP $ \x -> do
      popper <- liftIO $ feedInflate inf x
      yieldPopper popper
    do chunk <- liftIO $ finishInflate inf
       unless (B.null chunk) $ yield chunk
    return r

compress
    :: (MonadStream m, MonadIO (BaseMonad m))
    => Int
    -> WindowBits
    -> m B.ByteString B.ByteString r r
compress level config = do
    def <- liftIO $ initDeflate level config
    r <- forP $ \x -> do
      popper <- liftIO $ feedDeflate def x
      yieldPopper popper
    yieldPopper (finishDeflate def)
    return r

yieldPopper :: (MonadStream m, MonadIO (BaseMonad m)) => Popper -> m a B.ByteString u ()
yieldPopper pop = do
  x <- liftIO pop
  case x of
    Nothing -> return ()
    Just chunk -> yield chunk >> yieldPopper pop
