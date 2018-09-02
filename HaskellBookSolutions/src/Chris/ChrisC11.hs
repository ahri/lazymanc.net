module ChrisC11 where

chrisC11 :: IO ()
chrisC11 = do
    print "Doggies is a type constructor"
    print "The kind of Doggies is: * -> *"
    print "The kind of Doggies String is: *"
    print "The type of Husky 10 is: Num a => Doggies a"
    print "The type of Husky (10 :: Integer) is: Doggies Integer"
    print "The type of Mastiff 'Scooby Do' is: Doggies String"
    print "DogueDeBordeaux is a both a type and data constructor"
    print "The type of DogueDeBordeaux is: * -> *"
    print "The type of DogueDeBordeaux 'doggie' is: *"

-- We say that the type argument, a, is phantom or "has no witness"
data Phantom a = Phantom' -- Phantom' is a constant value

data Doggies a
    = Husky a
    | Mastiff a
    deriving (Eq, Show)

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Price = Price Integer 
    deriving (Eq, Show)

data Manufacturer 
    = Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline 
    = PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle 
    = Car Manufacturer Price
    | Plane Airline Integer
    deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir 1000

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car m _) = Just m 
getManu _ = Nothing

-- newtype

newtype Goats = Goats Int
    deriving (Eq, Show)

newtype Cows = Cows Int
    deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 54

-- newtypes allow us more power than type synonyms:
type Goats' = Int
tooManyGoats1 :: Goats' -> Bool
tooManyGoats1 n = n > 54
-- We are no longer controlling whether it is the number of goats or number of anything else

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 54

-- tooMany (55 :: Int)
-- > True

instance TooMany Goats where
    tooMany (Goats n) = n > 60

-- tooMany (Goats 55)
-- > False

data Person = MkPerson String Int

data PersonR = PersonR { name :: String
                       , age :: Int }
                       deriving (Eq, Show)

data BinaryTree a 
    = Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right) 

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay = 
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

