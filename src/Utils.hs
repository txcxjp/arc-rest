module Utils
  ( lbsToString,
  )
where

import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text
import Data.Text.Encoding (decodeUtf8)

lbsToString :: BL.ByteString -> String
lbsToString = Data.Text.unpack . decodeUtf8 . toStrict
