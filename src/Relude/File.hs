{- |
Copyright:  (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions to work with files for 'Text', 'LText', 'ByteString' and 'LByteString'
types.
-}

module Relude.File
       ( -- * Text
         readFileText
       , writeFileText
       , appendFileText

         -- * Lazy Text
       , readFileLText
       , writeFileLText
       , appendFileLText

         -- * ByteString
       , readFileBS
       , writeFileBS
       , appendFileBS

         -- * Lazy ByteString
       , readFileLBS
       , writeFileLBS
       , appendFileLBS
       ) where

import Relude.Base (FilePath, IO)
import Relude.Function ((.))
import Relude.Monad.Reexport (MonadIO (..))
import Relude.String (ByteString, LByteString, LText, Text)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT

----------------------------------------------------------------------------
-- Text
----------------------------------------------------------------------------

-- | Lifted version of 'T.readFile'.
readFileText :: MonadIO m => FilePath -> m Text
readFileText = liftIO . T.readFile
{-# SPECIALIZE readFileText :: FilePath -> IO Text #-}
{-# INLINE readFileText #-}

-- | Lifted version of 'T.writeFile'.
writeFileText :: MonadIO m => FilePath -> Text -> m ()
writeFileText p = liftIO . T.writeFile p
{-# SPECIALIZE writeFileText :: FilePath -> Text -> IO () #-}
{-# INLINE     writeFileText #-}

-- | Lifted version of 'T.appendFile'.
appendFileText :: MonadIO m => FilePath -> Text -> m ()
appendFileText p = liftIO . T.appendFile p
{-# SPECIALIZE appendFileText :: FilePath -> Text -> IO () #-}
{-# INLINE     appendFileText #-}

----------------------------------------------------------------------------
-- Lazy Text
----------------------------------------------------------------------------

-- | Lifted version of 'LT.readFile'.
readFileLText :: MonadIO m => FilePath -> m LText
readFileLText = liftIO . LT.readFile
{-# SPECIALIZE readFileLText :: FilePath -> IO LText #-}
{-# INLINE readFileLText #-}

-- | Lifted version of 'LT.writeFile'.
writeFileLText :: MonadIO m => FilePath -> LText -> m ()
writeFileLText p = liftIO . LT.writeFile p
{-# SPECIALIZE writeFileLText :: FilePath -> LText -> IO () #-}
{-# INLINE     writeFileLText #-}

-- | Lifted version of 'LT.appendFile'.
appendFileLText :: MonadIO m => FilePath -> LText -> m ()
appendFileLText p = liftIO . LT.appendFile p
{-# SPECIALIZE appendFileLText :: FilePath -> LText -> IO () #-}
{-# INLINE     appendFileLText #-}

----------------------------------------------------------------------------
-- ByteString
----------------------------------------------------------------------------

-- | Lifted version of 'BS.readFile'.
readFileBS :: MonadIO m => FilePath -> m ByteString
readFileBS = liftIO . BS.readFile
{-# SPECIALIZE readFileBS :: FilePath -> IO ByteString #-}
{-# INLINE readFileBS #-}

-- | Lifted version of 'BS.writeFile'.
writeFileBS :: MonadIO m => FilePath -> ByteString -> m ()
writeFileBS p = liftIO . BS.writeFile p
{-# SPECIALIZE writeFileBS :: FilePath -> ByteString -> IO () #-}
{-# INLINE     writeFileBS #-}

-- | Lifted version of 'BS.appendFile'.
appendFileBS :: MonadIO m => FilePath -> ByteString -> m ()
appendFileBS p = liftIO . BS.appendFile p
{-# SPECIALIZE appendFileBS :: FilePath -> ByteString -> IO () #-}
{-# INLINE     appendFileBS #-}

----------------------------------------------------------------------------
-- Lazy ByteString
----------------------------------------------------------------------------

-- | Lifted version of 'LBS.readFile'.
readFileLBS :: MonadIO m => FilePath -> m LByteString
readFileLBS = liftIO . LBS.readFile
{-# SPECIALIZE readFileLBS :: FilePath -> IO LByteString #-}
{-# INLINE readFileLBS #-}

-- | Lifted version of 'LBS.writeFile'.
writeFileLBS :: MonadIO m => FilePath -> LByteString -> m ()
writeFileLBS p = liftIO . LBS.writeFile p
{-# SPECIALIZE writeFileLBS :: FilePath -> LByteString -> IO () #-}
{-# INLINE     writeFileLBS #-}

-- | Lifted version of 'LBS.appendFile'.
appendFileLBS :: MonadIO m => FilePath -> LByteString -> m ()
appendFileLBS p = liftIO . LBS.appendFile p
{-# SPECIALIZE appendFileLBS :: FilePath -> LByteString -> IO () #-}
{-# INLINE     appendFileLBS #-}
