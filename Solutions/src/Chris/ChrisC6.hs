module ChrisC6 where

c6 :: IO ()
c6 = do
    let s1 = Sentence "Chris" "likes"
    let s2 = Sentence "Chris" "likes" "linguistics"
    return ()
    
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
    deriving (Eq, Show)

data Trivial = Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

data DayOfWeek
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun
    deriving (Show)

data Date = Date DayOfWeek Int
    deriving (Show)

instance Eq DayOfWeek where
    Mon == Mon = True
    Tue == Tue = True
    Wed == Wed = True
    Thu == Thu = True
    Fri == Fri = True
    Sat == Sat = True
    Sun == Sun = True
    _  == _ = False

instance Eq Date where
    (==) (Date weekday day) (Date weekday' day') = weekday == weekday' && day == day

-- :set -Wall

-- Exercises
-- 1 

data TisAnInteger = TisAn Integer -- not a typo lol

instance Eq TisAnInteger where
    (==) (TisAn n) (TisAn n') = n == n'

-- 2

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    Two n1 n2 == Two n1' n2' = (n1 == n1') && (n2 == n2')

-- 3

data StringOrInt 
    = TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
    TisAnInt n == TisAnInt n' = n == n'
    TisAString s == TisAString s' = s == s'
    TisAnInt _ == TisAString _ = False
    TisAString _ == TisAnInt _ = False

-- 4

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    Pair a1 a2 == Pair b1 b2 = (a1 == b1) && (a2 == b2)

-- 5

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where 
    (==) (Tuple a b) (Tuple a' b') = (a == a') && (b == b')    

-- 6

data Which a = ThisOne a | ThatOne a    

instance Eq a => Eq (Which a) where
    ThisOne a == ThisOne a' = a == a'
    ThatOne a == ThatOne a' = a == a'
    ThisOne _ == ThatOne _ = False
    ThatOne _ == ThisOne _ = False

-- 7

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    Hello a == Hello a' = a == a'
    Goodbye b == Goodbye b' = b == b'
    Hello _ == Goodbye _ = False
    Goodbye _ == Hello _ = False



