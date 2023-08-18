module Dto.Tag where

import Data.Text (Text)

newtype CreateTagReq = CreateTagReq
  { value :: Text
  }

data SearchTagsReq = SearchTagsReq
  { maximum :: Int
  , searchPhrase :: Text
  }
