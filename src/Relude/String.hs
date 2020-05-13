{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

Type classes for conversion between different string representations.

The table below represents the @relude@ concept of conversion between the following types:

* 'Text'
* 'String'
* 'ByteString'
* 'LText'
* 'LByteString'
* 'ShortByteString'


+-------------------+---------------------+--------------+--------------+--------------------+------------------------+--------------------------+
|     From \\ To    |       'Text'        |   'String'   | 'ByteString' |      'LText'       | 'LByteString'          | 'ShortByteString'        |
+===================+=====================+==============+==============+====================+========================+==========================+
| 'Text'            | ~                   | 'toString'   | 'encodeUtf8' | 'toLazy'/'toLText' | 'encodeUtf8'           | 'encodeUtf8'             |
+-------------------+---------------------+--------------+--------------+--------------------+------------------------+--------------------------+
| 'String'          | 'toText'            | ~            | 'encodeUtf8' | 'toLText'          | 'encodeUtf8'           | 'encodeUtf8'             |
+-------------------+---------------------+--------------+--------------+--------------------+------------------------+--------------------------+
| 'ByteString'      | 'decodeUtf8'        | 'decodeUtf8' | ~            | 'decodeUtf8'       | 'toLazy'               | 'toShort'                |
+-------------------+---------------------+--------------+--------------+--------------------+------------------------+--------------------------+
| 'LText'           | 'toStrict'/'toText' | 'toString'   | 'encodeUtf8' |  ~                 | 'encodeUtf8'           | 'encodeUtf8'             |
+-------------------+---------------------+--------------+--------------+--------------------+------------------------+--------------------------+
| 'LByteString'     | 'decodeUtf8'        | 'decodeUtf8' | 'toStrict'   | 'decodeUtf8'       | ~                      | @'toShort' . 'toStrict'@ |
+-------------------+---------------------+--------------+--------------+--------------------+------------------------+--------------------------+
| 'ShortByteString' | 'decodeUtf8'        | 'decodeUtf8' | 'fromShort'  | 'decodeUtf8'       |@'toLazy' . 'fromShort'@| ~                        |
+-------------------+---------------------+--------------+--------------+--------------------+------------------------+--------------------------+

-}

module Relude.String
    ( module Relude.String.Reexport
      -- $reexport
    , module Relude.String.Conversion
      -- $conversion
    ) where

import Relude.String.Conversion
import Relude.String.Reexport

{- $reexport
Reexport data types and functions to work with 'Text', 'ByteString',
'ShortByteString'.
-}
{- $conversion
Conversion functions between 'Text', 'String', 'ByteString'.
-}
