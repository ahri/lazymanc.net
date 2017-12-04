module C12 where

c12 :: IO ()
c12 = do
    print $ replaceThe "the cow loves us"
    print $ replaceThe' "the cow loves us"
    print $ replaceThe'' "the cow loves us"
    print $ replaceThe''' "the cow loves us"

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x

replaceThe :: String -> String
replaceThe []               = []
replaceThe ('t':'h':'e':xs) = 'a' : replaceThe xs
replaceThe (x:xs)           = x : replaceThe xs


replaceThe' :: String -> String
replaceThe' = unwords . map (unroll . notThe) . words
    where
        unroll Nothing  = "a"
        unroll (Just x) = x


replaceThe'' :: String -> String
replaceThe'' = unwords . map (unroll . notThe) . words
    where
        unroll = maybe "a" id


replaceThe''' :: String -> String
replaceThe''' = unwords . recurse . map notThe . words
    where
        recurse []             = []
        recurse (Nothing : xs) = "a" : recurse xs
        recurse (Just x : xs)  = x : recurse xs

maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' d _ Nothing  = d
maybe' _ f (Just x) = f x
