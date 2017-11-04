module C6 where

c6 :: IO ()
c6 = do
    putStrLn "Eq Instances"
    print (TisAn 5 == TisAn 5)
    print (Two 5 6 == Two 5 6)
    print (TisAnInt 5 == TisAnInt 5)
    print (TisAString "foo" == TisAString "foo")
    print (TisAString "foo" == TisAString "fo")
    print (TisAString "foo" == TisAnInt 7)

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn y) = x == y

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') = x == x' && y == y'

data StringOrInt
    = TisAnInt Int
    | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt y)     = x == y
    (==) (TisAString x) (TisAString y) = x == y
    (==) _ _                           = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne x) (ThatOne y) = x == y
    (==) _ _                     = False

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello y)     = x == y
    (==) (Goodbye x) (Goodbye y) = x == y
    (==) _ _                     = False

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fab a b = fab a == b

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith fab i a = fab a + fromIntegral i
