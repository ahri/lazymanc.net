module Max where

-- Breaks a string into words
split :: String -> [String]
split str = split' str [""]
    where
        split' [] wrds = reverse wrds
        split' (' ':str) wrds = split' str ([]:wrds)
        split' (char:str) (wrd:wrds) = split' str ((wrd ++ [char]):wrds)

-- Replace "the" for "a" in a string
replaceThe :: String -> String
replaceThe inputStr = replaceThe' (split inputStr) ""
    where
        replaceThe' [] outputStr = outputStr
        replaceThe' ("the":wrds) outputStr = "a " ++ (replaceThe' wrds outputStr)
        replaceThe' (wrd:wrds) outputStr = wrd ++ " " ++ (replaceThe' wrds outputStr)

-- Counts "the"s followed by vowel
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = count (split str) 0
    where
        count [] i = i
        count ("the":wrds) i = count wrds (incIfTrue (startsWithVowel wrds) i)
        count (x:xs) i = count xs i
        startsWithVowel ((char:_):_) = elem char ['a', 'e', 'i', 'o', 'u']
        incIfTrue True i = i + 1
        incIfTrue False i = i

main :: IO ()
main = do
    putStrLn "Chapter 12"
    putStrLn "--replaceThe--"
    putStrLn (replaceThe "the cow loves us. the fish loves the cow. dilema")
    putStrLn "--countTheBeforeVowel--"
    putStrLn (show (countTheBeforeVowel "the cow, the evil cow, the owl and the evil owl"))
