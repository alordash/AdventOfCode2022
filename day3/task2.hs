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

groupByCount :: Int -> [a] -> [[a]]
groupByCount _ [] = []
groupByCount n l = left : groupByCount n right
  where
    (left, right) = splitAt n l

findCommonCharsAcrossThree [] = []
findCommonCharsAcrossThree [x, y, z] =
  toList $ findCommonChars z $ toList (findCommonChars x y)

main = do
  lines <- inputLoop
  let groupsBy3 = groupByCount 3 lines
  let commonChars = map findCommonCharsAcrossThree groupsBy3
  print commonChars
  let priorities = sum $ map (head . map charPriority) commonChars
  print priorities
