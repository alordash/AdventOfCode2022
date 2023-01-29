import qualified Data.Maybe
import           Data.Text          (strip)
import           Debug.Trace        (trace)
import           GHC.Settings.Utils (maybeRead)

inputLoop :: IO [String]
inputLoop = do
  line <- getLine
  if line == "."
    then return []
    else do
      rest <- inputLoop
      return (line : rest)

groupByCount :: Int -> [a] -> [[a]]
groupByCount _ [] = []
groupByCount n l = left : groupByCount n rest
  where
    (left, rest) = splitAt n l

data ParseResult
  = Crates [Char]
  | Numbers
  deriving (Show, Read, Eq)

isCrates :: ParseResult -> Bool
isCrates Numbers = False
isCrates _       = True

trim :: String -> String
trim [] = []
trim s  = (unwords . words) s

tryParseInt :: [String] -> Maybe Int
tryParseInt strings = maybeRead (trim $ head strings)

parseCrates :: [String] -> ParseResult
parseCrates [] = Crates []
parseCrates groups =
  case maybeInt of
    Just _  -> Numbers
    Nothing -> Crates (map (!! 1) groups)
  where
    maybeInt = tryParseInt groups

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x      = map head x : transpose (map tail x)

data Move =
  Move
    { count :: Int
    , from  :: Int
    , to    :: Int
    }
  deriving (Show, Read, Eq)

parseMove :: String -> Move
parseMove s = Move {count = count, from = from, to = to}
  where
    ws = words s
    count = read (ws !! 1)
    from = read (ws !! 3) - 1
    to = read (ws !! 5) - 1

mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed _ [] = []
mapIndexed f l = propagate 0 f l
  where
    propagate :: Int -> (Int -> a -> b) -> [a] -> [b]
    propagate _ _ []       = []
    propagate idx f (x:xs) = f idx x : propagate (idx + 1) f xs

debug = flip trace

applyMove :: [String] -> Move -> [String]
applyMove stacks Move {count = count, from = from, to = to} =
  mapIndexed
    (\idx v -> processStack idx Move {count = count, from = from, to = to} v)
    stacks
  where
    processStack :: Int -> Move -> String -> String
    processStack idx Move {count = count, from = from, to = to} stack
      | idx == from = drop count stack
      | idx == to = movedCrates ++ stack
      | otherwise = stack
    movedCrates = reverse $ take count (stacks !! from)

main = do
  lines <- inputLoop
  let groups = map (groupByCount 4) lines
  let cratesLines =
        map (\(Crates c) -> c) $ takeWhile isCrates (map parseCrates groups)
  let stacks = map trim $ transpose cratesLines
  print stacks
  let moveLines = drop (length cratesLines + 2) lines
  let moves = map parseMove moveLines
  let movedStacks = foldl applyMove stacks moves
  let result = map head movedStacks
  print result
