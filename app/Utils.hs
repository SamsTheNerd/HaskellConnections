
module Utils where
import Data.List (sortBy)
import Data.Char (ord)


-- makes an arbitrary enough hash of a word to appear semi-random while staying pure
hashWord :: String -> Int
hashWord str = ((`div` length str) .sum . map ((77 -) . ord)) str

-- shuffles a list of words based on hashWord to appear semi-random while staying pure
shuffle :: [String] -> [String]
shuffle = sortBy (\a b -> compare (hashWord a) (hashWord b))

-- group a list into chunks of size n. the last chunk may be shorter. returns [] if the input is []
chunk :: Int -> [a] -> [[a]]
chunk n xs = if null xs then [] else 
    take n xs : if null xss then [] else chunk n xss
    where xss = drop n xs

-- applies the function each value matching the predicate
replace :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replace p f = map (\x -> if p x then f x else x)
