module C11 where
import C09
import Data.Char
import Data.List

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 15)

vs = [myCar, urCar, clownCar, doge]

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = undefined

class TooMany a where
	tooMany :: a -> Bool
	
instance TooMany Int where
	tooMany n = n > 42

data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows deriving (Eq, Show)
data ProgLang = Haskell | Agda | Idris | PureScript deriving (Eq, Show)
data Programmer = Programmer { os :: OperatingSystem, lang :: ProgLang } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer{ lang = l, os = o } | l <- allLanguages, o <- allOperatingSystems]

-- length allOperatingSystems * length allLanguages == length allProgrammers

-------- map for BinaryTree --------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b 
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay = if mapTree (+1) testTree' == mapExpected then print "yup okay!" else error "test failed!"

-------- BinaryTree2List --------------
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left val right) = concat [[val], preorder left, preorder right]

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left val right) = concat [inorder left, [val], inorder right]

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left val right) = concat [postorder left, postorder right, [val]]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2] then putStrLn "Postorder fine!" else putStrLn "postorder failed check"

-------- BinaryTree foldr --------------
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f init tree = foldr f init (preorder tree)

-------- Vigenere --------------
indexOf :: Eq a => a -> [a] -> Int -> Int 	-- assumes existence
indexOf _ [] _ = -1
indexOf item (x:xs) startFrom = if x == item then startFrom else indexOf item xs startFrom+1

stretchKey plain key = (concat $ replicate ((quot (length plain) (length key)) + 1) key)

vigenere :: (String -> Int -> String) -> String -> String -> String
vigenere _ [] _ = ""
vigenere _ _ [] = ""
vigenere f (first:plain) key = do
	let dupKey = stretchKey (first:plain) key
	let firstK = head dupKey
	let insertSpace = isSpace $ head plain	-- massive hack to support spaces
	let abc = if isUpper firstK then ['A'..'Z'] else if isDigit firstK then ['0'..'9'] else ['a'..'z']
	f [first] (indexOf firstK abc (length abc)) ++ vigenere f plain (if insertSpace then dupKey else tail dupKey)
	
vigenereCipher :: String -> String -> String
vigenereCipher plain key = vigenere cipher plain key
	
vigenereDecipher :: String -> String -> String
vigenereDecipher plain key = vigenere decipher plain key