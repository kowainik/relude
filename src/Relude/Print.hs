{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy       #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions like 'Prelude.putStr' and 'Prelude.putStrLn' but for 'Text', 'LText',
'ByteString' and 'LByteString'.
-}

module Relude.Print
       ( -- * 'Text' & 'LText'
         putText
       , putTextLn
       , putLText
       , putLTextLn

         -- * 'ByteString' & 'LByteString'
       , putBS
       , putBSLn
       , putLBS
       , putLBSLn
       ) where

import Relude.Function ((.))
import Relude.Monad.Reexport (MonadIO (..))
import Relude.String (ByteString, LByteString, LText, Text)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Relude.Base as Base

----------------------------------------------------------------------------
-- Text
----------------------------------------------------------------------------

{- | Lifted version of 'T.putStr'.

>>> putText "Hello, world!"
Hello, world!
-}
putText :: MonadIO m => Text -> m ()
putText = liftIO . T.putStr
{-# SPECIALIZE putText :: Text -> Base.IO () #-}
{-# INLINE putText #-}


{- | Lifted version of 'T.putStrLn'.

>>> putTextLn "Hello, world!"
Hello, world!
-}
putTextLn :: MonadIO m => Text -> m ()
putTextLn = liftIO . T.putStrLn
{-# SPECIALIZE putTextLn :: Text -> Base.IO () #-}
{-# INLINE putTextLn #-}

{- | Lifted version of 'TL.putStr'.

>>> putLText "Hello, world!"
Hello, world!
-}
putLText :: MonadIO m => LText -> m ()
putLText = liftIO . TL.putStr
{-# SPECIALIZE putLText :: LText -> Base.IO () #-}
{-# INLINE putLText #-}

{- | Lifted version of 'TL.putStrLn'.

>>> putLTextLn "Hello, world!"
Hello, world!
-}
putLTextLn :: MonadIO m => LText -> m ()
putLTextLn = liftIO . TL.putStrLn
{-# SPECIALIZE putLTextLn :: LText -> Base.IO () #-}
{-# INLINE putLTextLn #-}

----------------------------------------------------------------------------
-- ByteString
----------------------------------------------------------------------------

{- | Lifted version of 'BS.putStr'.

>>> putBS ("Hello, world!" :: ByteString)
Hello, world!
-}
putBS :: MonadIO m => ByteString -> m ()
putBS = liftIO . BS.putStr
{-# SPECIALIZE putBS :: ByteString -> Base.IO () #-}
{-# INLINE putBS #-}


{- | Lifted version of 'BS.putStrLn'.

>>> putBSLn ("Hello, world!" :: ByteString)
Hello, world!

-}
putBSLn :: MonadIO m => ByteString -> m ()
putBSLn = liftIO . BS.putStrLn
{-# SPECIALIZE putBSLn :: ByteString -> Base.IO () #-}
{-# INLINE putBSLn #-}

{- | Lifted version of 'LBS.putStr'.

>>> putLBS ("Hello, world!" :: LByteString)
Hello, world!

-}
putLBS :: MonadIO m => LByteString -> m ()
putLBS = liftIO . LBS.putStr
{-# SPECIALIZE putLBS :: LByteString -> Base.IO () #-}
{-# INLINE putLBS #-}

{- | Lifted version of 'LBS.putStrLn'.

>>> putLBSLn ("Hello, world!" :: LByteString)
Hello, world!

-}
putLBSLn :: MonadIO m => LByteString -> m ()
putLBSLn = liftIO . LBS.putStrLn
{-# SPECIALIZE putLBSLn :: LByteString -> Base.IO () #-}
{-# INLINE putLBSLn #-}
