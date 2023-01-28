data Range =
  Range Int Int
  deriving (Read, Show, Eq)

rangeContainsRange :: Range -> Range -> Bool
rangeContainsRange (Range beginFirst endFirst) (Range beginSecond endSecond) =
  (beginFirst <= beginSecond && beginSecond <= endFirst) ||
  (beginFirst <= endSecond && endSecond <= endFirst)

rangesOverlap :: Range -> Range -> Bool
rangesOverlap a b = rangeContainsRange a b || rangeContainsRange b a

inputLoop :: IO [String]
inputLoop = do
  line <- getLine
  if line == "."
    then return []
    else do
      rest <- inputLoop
      return (line : rest)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p l =
  case dropWhile p l of
    [] -> []
    suitable ->
      case splitBy p rest of
        []        -> [head]
        something -> head : something
      where (head, rest) = break p suitable

stringToRange :: String -> Range
stringToRange s = toRange $ map read (splitBy (== '-') s)
  where
    toRange [x, y] = Range x y

tuplify [a, b] = (a, b)

main = do
  lines <- inputLoop
  let rangePairs = map (tuplify . map stringToRange . splitBy (== ',')) lines
  print rangePairs
  let overlaps = map (uncurry rangesOverlap) rangePairs
  print overlaps
  let overlapsCount = length $ filter (== True) overlaps
  print overlapsCount
