module Utils (throwLeft) where

throwLeft :: Either String b -> b
throwLeft (Left msg) = error msg
throwLeft (Right v) = v
