module C05 where

c05 :: IO ()
c05 = do
    putStrLn "5.4 Type Arguments"
    print $ 3.5 `plus` 2.7 -- seems to work fine without NoMonomorphismRestriction
    where
        plus = (+)
        c5_ta_1_h :: Int -> Char
        c5_ta_1_h = g . f
            where
                f :: Int -> String
                f = undefined
                g :: String -> Char
                g = undefined

        c5_ta_2_e :: A -> C
        c5_ta_2_e = w . q
            where
                q :: A -> B
                q = undefined
                w :: B -> C
                w = undefined

        c5_ta_3_xform :: (X, Y) -> (Z, Z)
        c5_ta_3_xform (x, y) = (xz x, yz y)
            where
                xz :: X -> Z
                xz = undefined
                yz :: Y -> Z
                yz = undefined

        c5_ta_4_munge :: (x -> y)
                      -> (y -> (w, z))
                      -> x
                      -> w
        c5_ta_4_munge x_y y_wz = fst . y_wz . x_y



data A
data B
data C

data X
data Y
data Z
