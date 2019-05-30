module Safe where

nth :: [a] -> Int -> Maybe a
nth arr n = if n >= (length arr) then Nothing else return $ arr!!n

fromMaybe :: a -> Maybe a -> a
fromMaybe def (Just a) = a
fromMaybe def Nothing = def

proceed :: (a -> b) -> b -> Maybe a -> b
proceed success failure (Just a) = success a
proceed success failure Nothing = failure
