unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)


map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail