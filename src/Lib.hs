module Lib
    ( compile
    ) where

import qualified Data.ByteString.Lazy.Char8 as BS

compile :: BS.ByteString -> BS.ByteString
compile input = input
