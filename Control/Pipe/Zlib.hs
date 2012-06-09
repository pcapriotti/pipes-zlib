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
import Control.Pipe.Combinators
import qualified Data.ByteString as B
import Prelude hiding (catch)

-- | Gzip compression with default parameters.
gzip :: MonadIO m => Pipe l B.ByteString B.ByteString m r
gzip = compress 1 (WindowBits 31)

-- | Gzip decompression with default parameters.
gunzip :: MonadIO m => Pipe l B.ByteString B.ByteString m r
gunzip = decompress (WindowBits 31)

decompress
    :: MonadIO m
    => WindowBits
    -> Pipe l B.ByteString B.ByteString m r
decompress config = do
    inf <- liftIO $ initInflate config
    forP $ \x -> do
      popper <- liftIO $ feedInflate inf x
      yieldPopper popper
    chunk <- liftIO $ finishInflate inf
    unless (B.null chunk) $ yield chunk
    discard

compress
    :: MonadIO m
    => Int
    -> WindowBits
    -> Pipe l B.ByteString B.ByteString m r
compress level config = do
    def <- liftIO $ initDeflate level config
    forP $ \x -> do
      popper <- liftIO $ feedDeflate def x
      yieldPopper popper
    yieldPopper (finishDeflate def)
    discard

yieldPopper :: MonadIO m => Popper -> Pipe l a B.ByteString m ()
yieldPopper pop = do
  x <- liftIO pop
  case x of
    Nothing -> return ()
    Just chunk -> yield chunk >> yieldPopper pop
