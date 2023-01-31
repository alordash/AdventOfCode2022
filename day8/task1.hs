import           Data.Char (ord)

inputLoop :: IO [String]
inputLoop = do
  line <- getLine
  if line == "."
    then return []
    else do
      rest <- inputLoop
      return (line : rest)

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xs     = map head xs : transpose (map tail xs)

data Tree =
  Tree Int Bool
  deriving (Show, Read, Eq)

toTrees :: [Int] -> [Tree]
toTrees = map (`Tree` False)

charToInt :: Char -> Int
charToInt c = ord c - ord '0'

toHeights :: String -> [Int]
toHeights = map charToInt

processVisibleTrees :: Int -> [Tree] -> [Tree]
processVisibleTrees _ [] = []
processVisibleTrees heighestTree (t:ts) =
  if treeHeight > heighestTree
    then Tree treeHeight True : restTrees
    else Tree treeHeight treeVisited : restTrees
  where
    (Tree treeHeight treeVisited) = t
    newHeighestTree = max heighestTree treeHeight
    restTrees = processVisibleTrees newHeighestTree ts

countVisibleTrees :: [[Tree]] -> (Int, [[Tree]])
countVisibleTrees [] = (-1, [])
countVisibleTrees trees = (leftVisitsCount, leftTrees)
  where
    leftTrees = map (processVisibleTrees (-1)) trees
    leftVisitsCount =
      length $
      filter (== True) $
      concatMap (map (\(Tree _ visited) -> visited)) leftTrees

main = do
  lines <- inputLoop
  let trees = map (toTrees . toHeights) lines
  let treesForLeft = trees
  let (leftVisibleCount, leftTrees) = countVisibleTrees treesForLeft
  let treesForRight = map reverse leftTrees
  let (leftRightVisibleCount, rightTrees) = countVisibleTrees treesForRight
  let treesForTop = transpose rightTrees
  let (leftRightTopVisibleCount, topTrees) = countVisibleTrees treesForTop
  let treesForDown = map reverse topTrees
  let (leftRightTopDownVisibleCount, downTrees) = countVisibleTrees treesForDown
  -- print downTrees
  let originalTrees = map reverse $ transpose $ map reverse downTrees
  print originalTrees
  print leftRightTopDownVisibleCount
