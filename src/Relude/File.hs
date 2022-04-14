{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2018-2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Lifted to 'MonadIO' families of file processing functions for the 'Text', 'LText',
'ByteString' and 'LByteString' types.

These functions are lifted which means that you can also use them inside
various Monad Transformers without adding 'liftIO' call explicitly.

__NOTE:__ These functions are for working with textual data. Functions that work
with 'Text' or 'LText' types are system and locale-sensitive (encoding,
line-endings). If you want binary data, use 'ByteString' functions (they are
also faster since they don't check encoding). However, you can then decode that
data with the help of functions from the "Relude.String.Conversion" module, e. g.
'Relude.String.Conversion.decodeUtf8'.

To be more precise, avoid the following functions:

* 'readFileText'
* 'readFileLText'

See the following blog post for more details:

* [Beware of readFile by Michael Snoyman](https://www.snoyman.com/blog/2016/12/beware-of-readfile/)

@since 0.3.0
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

@since 0.3.0
-}
readFileText :: MonadIO m => FilePath -> m Text
readFileText = liftIO . T.readFile
{-# SPECIALIZE readFileText :: FilePath -> IO Text #-}
{-# INLINE     readFileText #-}
{-# WARNING readFileText ["'readFileText' depends on the system's locale settings and can throw unexpected exceptions.", "Use 'readFileBS' instead."] #-}

{- | Lifted version of 'T.writeFile'.

@since 0.3.0
-}
writeFileText :: MonadIO m => FilePath -> Text -> m ()
writeFileText p = liftIO . T.writeFile p
{-# SPECIALIZE writeFileText :: FilePath -> Text -> IO () #-}
{-# INLINE     writeFileText #-}

{- | Lifted version of 'T.appendFile'.

@since 0.3.0
-}
appendFileText :: MonadIO m => FilePath -> Text -> m ()
appendFileText p = liftIO . T.appendFile p
{-# SPECIALIZE appendFileText :: FilePath -> Text -> IO () #-}
{-# INLINE     appendFileText #-}

----------------------------------------------------------------------------
-- Lazy Text
----------------------------------------------------------------------------

{- | Lifted version of 'LT.readFile'.

@since 0.3.0
-}
readFileLText :: MonadIO m => FilePath -> m LText
readFileLText = liftIO . LT.readFile
{-# SPECIALIZE readFileLText :: FilePath -> IO LText #-}
{-# INLINE     readFileLText #-}
{-# WARNING readFileLText ["'readFileLText' depends on the system's locale settings and can throw unexpected exceptions.", "Use 'readFileLBS' instead."] #-}

{- | Lifted version of 'LT.writeFile'.

@since 0.3.0
-}
writeFileLText :: MonadIO m => FilePath -> LText -> m ()
writeFileLText p = liftIO . LT.writeFile p
{-# SPECIALIZE writeFileLText :: FilePath -> LText -> IO () #-}
{-# INLINE     writeFileLText #-}

{- | Lifted version of 'LT.appendFile'.

@since 0.3.0
-}
appendFileLText :: MonadIO m => FilePath -> LText -> m ()
appendFileLText p = liftIO . LT.appendFile p
{-# SPECIALIZE appendFileLText :: FilePath -> LText -> IO () #-}
{-# INLINE     appendFileLText #-}

----------------------------------------------------------------------------
-- ByteString
----------------------------------------------------------------------------

{- | Lifted version of 'BS.readFile'.

@since 0.3.0
-}
readFileBS :: MonadIO m => FilePath -> m ByteString
readFileBS = liftIO . BS.readFile
{-# SPECIALIZE readFileBS :: FilePath -> IO ByteString #-}
{-# INLINE     readFileBS #-}

{- | Lifted version of 'BS.writeFile'.

@since 0.3.0
-}
writeFileBS :: MonadIO m => FilePath -> ByteString -> m ()
writeFileBS p = liftIO . BS.writeFile p
{-# SPECIALIZE writeFileBS :: FilePath -> ByteString -> IO () #-}
{-# INLINE     writeFileBS #-}

{- | Lifted version of 'BS.appendFile'.

@since 0.3.0
-}
appendFileBS :: MonadIO m => FilePath -> ByteString -> m ()
appendFileBS p = liftIO . BS.appendFile p
{-# SPECIALIZE appendFileBS :: FilePath -> ByteString -> IO () #-}
{-# INLINE     appendFileBS #-}

----------------------------------------------------------------------------
-- Lazy ByteString
----------------------------------------------------------------------------

{- | Lifted version of 'LBS.readFile'.

@since 0.3.0
-}
readFileLBS :: MonadIO m => FilePath -> m LByteString
readFileLBS = liftIO . LBS.readFile
{-# SPECIALIZE readFileLBS :: FilePath -> IO LByteString #-}
{-# INLINE     readFileLBS #-}

{- | Lifted version of 'LBS.writeFile'.

@since 0.3.0
-}
writeFileLBS :: MonadIO m => FilePath -> LByteString -> m ()
writeFileLBS p = liftIO . LBS.writeFile p
{-# SPECIALIZE writeFileLBS :: FilePath -> LByteString -> IO () #-}
{-# INLINE     writeFileLBS #-}

{- | Lifted version of 'LBS.appendFile'.

@since 0.3.0
-}
appendFileLBS :: MonadIO m => FilePath -> LByteString -> m ()
appendFileLBS p = liftIO . LBS.appendFile p
{-# SPECIALIZE appendFileLBS :: FilePath -> LByteString -> IO () #-}
{-# INLINE     appendFileLBS #-}
