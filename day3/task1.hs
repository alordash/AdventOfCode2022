import           Data.Char (ord)
import           Data.Set  (Set, empty, fromList, toList, union)

inputLoop :: IO [String]
inputLoop = do
  line <- getLine
  if line == "."
    then return []
    else do
      rest <- inputLoop
      return (line : rest)

splitString :: String -> (String, String)
splitString s = splitAt halfLength s
  where
    halfLength = length s `div` 2

findCommonChars :: String -> String -> Set Char
findCommonChars [] _ = empty
findCommonChars (x:xs) s = first `union` findCommonChars xs s
  where
    first = fromList (filter (== x) s)

charPriority :: Char -> Int
charPriority c
  | v <= ordZ = v - ordA + ordz - orda + 2
  | otherwise = v - orda + 1
  where
    orda = ord 'a'
    ordz = ord 'z'
    ordA = ord 'A'
    ordZ = ord 'Z'
    v = ord c

main = do
  lines <- inputLoop
  let stringPairs = map splitString lines
  let commonChars = map (toList . uncurry findCommonChars) stringPairs
  let priorities = sum $ map (head . map charPriority) commonChars
  print priorities
