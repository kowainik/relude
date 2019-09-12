{- |
Copyright:  (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Lifted to 'MonadIO' families of file processing functions for 'Text', 'LText',
'ByteString' and 'LByteString' types.

__NOTE:__ These functions are for working with textual data. Functions that work
with 'Text' or 'LText' types are system and locale-sensitive (encoding,
line-endings). If you want binary data, use 'ByteString' functions (they are
also faster since they don't check encoding). However, you can then decode that
data with the help of functions from the @"Relude.String.Conversion"@ module, e. g.
'Relude.String.Conversion.decodeUtf8'.
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

{- | Lifted version of 'T.readFile'.

'T.readFile' is a function usually used when you want to read files inside IO monad,
but this 'readFileText' is lifted which means that you can use it even inside various
monad transformers without adding liftIO call explicitly.

-}
readFileText :: MonadIO m => FilePath -> m Text
readFileText = liftIO . T.readFile
{-# SPECIALIZE readFileText :: FilePath -> IO Text #-}
{-# INLINE     readFileText #-}

{- | Lifted version of 'T.writeFile'.

'T.writeFile' is a function usually used when you want to readwrite files inside IO monad,
but this 'writeFileText' is lifted which means that you can use it even inside various
monad transformers without adding liftIO call explicitly.

-}
writeFileText :: MonadIO m => FilePath -> Text -> m ()
writeFileText p = liftIO . T.writeFile p
{-# SPECIALIZE writeFileText :: FilePath -> Text -> IO () #-}
{-# INLINE     writeFileText #-}


{- | Lifted version of 'T.appendFile'.

'T.appendFile' is a function usually used when you want to appendFile inside IO monad,
but this 'appendFileText' is lifted which means that you can use it even inside various
monad transformers without adding liftIO call explicitly.

-}
appendFileText :: MonadIO m => FilePath -> Text -> m ()
appendFileText p = liftIO . T.appendFile p
{-# SPECIALIZE appendFileText :: FilePath -> Text -> IO () #-}
{-# INLINE     appendFileText #-}

----------------------------------------------------------------------------
-- Lazy Text
----------------------------------------------------------------------------

{- | Lifted version of 'LT.readFile'.

'LT.readFile' is a function usually used when you want to read files lazily inside IO monad,
but this 'readFileLText' is lifted which means that you can use it even inside various
monad transformers without adding liftIO call explicitly.

-}
readFileLText :: MonadIO m => FilePath -> m LText
readFileLText = liftIO . LT.readFile
{-# SPECIALIZE readFileLText :: FilePath -> IO LText #-}
{-# INLINE     readFileLText #-}


{- | Lifted version of 'LT.writeFile'.

'LT.writeFile' is a function usually used when you want to read files lazily inside IO monad,
but this 'writeFileLText' is lifted which means that you can use it even inside various
monad transformers without adding liftIO call explicitly.

-}
writeFileLText :: MonadIO m => FilePath -> LText -> m ()
writeFileLText p = liftIO . LT.writeFile p
{-# SPECIALIZE writeFileLText :: FilePath -> LText -> IO () #-}
{-# INLINE     writeFileLText #-}

{- | Lifted version of 'LT.appendFile'.

'LT.writeFile' is a function usually used when you want to appendFile lazily inside IO monad,
but this 'appendFileLText' is lifted which means that you can use it even inside various
monad transformers without adding liftIO call explicitly.

-}
appendFileLText :: MonadIO m => FilePath -> LText -> m ()
appendFileLText p = liftIO . LT.appendFile p
{-# SPECIALIZE appendFileLText :: FilePath -> LText -> IO () #-}
{-# INLINE     appendFileLText #-}

----------------------------------------------------------------------------
-- ByteString
----------------------------------------------------------------------------

{- | Lifted version of 'BS.readFile'.

'BS.readFile' is a function usually used when you want to read a file to 'ByteString'
inside IO monad, but this 'readFileBS' is lifted which means that you can use it even inside
various monad transformers without adding liftIO call explicitly.

-}
readFileBS :: MonadIO m => FilePath -> m ByteString
readFileBS = liftIO . BS.readFile
{-# SPECIALIZE readFileBS :: FilePath -> IO ByteString #-}
{-# INLINE     readFileBS #-}

{- | Lifted version of 'BS.writeFile'.

'BS.readFile' is a function usually used when you want to write a file to 'ByteString'
inside IO monad, but this 'writeFileBS' is lifted which means that you can use it even inside
various monad transformers without adding liftIO call explicitly.

-}
writeFileBS :: MonadIO m => FilePath -> ByteString -> m ()
writeFileBS p = liftIO . BS.writeFile p
{-# SPECIALIZE writeFileBS :: FilePath -> ByteString -> IO () #-}
{-# INLINE     writeFileBS #-}

{- | Lifted version of 'BS.appendFile'.

'BS.readFile' is a function usually used when you want to append a file using a 'ByteString'
inside IO monad, but this 'appendFileBS' is lifted which means that you can use it even inside
various monad transformers without adding liftIO call explicitly.

-}
appendFileBS :: MonadIO m => FilePath -> ByteString -> m ()
appendFileBS p = liftIO . BS.appendFile p
{-# SPECIALIZE appendFileBS :: FilePath -> ByteString -> IO () #-}
{-# INLINE     appendFileBS #-}

----------------------------------------------------------------------------
-- Lazy ByteString
----------------------------------------------------------------------------

{- | Lifted version of 'LBS.readFile'.

'LBS.readFile' is a function usually used when you want to read a file to 'ByteString' lazily
inside IO monad, but this 'readFileLBS' is lifted which means that you can use it even inside
various monad transformers without adding liftIO call explicitly.

-}
readFileLBS :: MonadIO m => FilePath -> m LByteString
readFileLBS = liftIO . LBS.readFile
{-# SPECIALIZE readFileLBS :: FilePath -> IO LByteString #-}
{-# INLINE     readFileLBS #-}

-- | Lifted version of 'LBS.writeFile'.
{- | Lifted version of 'LBS.writeFile'.

'LBS.writeFile' is a function usually used when you want to write a file using 'ByteString' lazily
inside IO monad, but this 'writeFileLBS' is lifted which means that you can use it even inside
various monad transformers without adding liftIO call explicitly.

-}
writeFileLBS :: MonadIO m => FilePath -> LByteString -> m ()
writeFileLBS p = liftIO . LBS.writeFile p
{-# SPECIALIZE writeFileLBS :: FilePath -> LByteString -> IO () #-}
{-# INLINE     writeFileLBS #-}


{- | Lifted version of 'LBS.appendFile'.

'LBS.appendFile' is a function usually used when you want to append a file with a 'ByteString' lazily
inside IO monad, but this 'appendFileLBS' is lifted which means that you can use it even inside various
monad transformers without adding liftIO call explicitly.

-}
appendFileLBS :: MonadIO m => FilePath -> LByteString -> m ()
appendFileLBS p = liftIO . LBS.appendFile p
{-# SPECIALIZE appendFileLBS :: FilePath -> LByteString -> IO () #-}
{-# INLINE     appendFileLBS #-}
