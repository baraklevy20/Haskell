
-- |
-- a utility module
module CPU.Utils where

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left  x) = Left (f x)
mapLeft _ (Right x) = Right x

map2 :: (a -> b) -> (a, a) -> (b, b)
map2 f (x,y) = (f x, f y)
