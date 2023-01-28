import           Control.DeepSeq    (deepseq)
import           Control.Monad      (join)
import           Data.List
import           GHC.Data.ShortText (ShortText (contents))
import           Text.Read

split :: (a -> Bool) -> [a] -> [[a]]
split p s =
  case dropWhile p s of
    [] -> [[]]
    s' -> w : split p s''
      where (w, s'') = break p s'

inputLoop :: IO [String]
inputLoop = do
  line <- getLine
  if line == "."
    then return []
    else do
      rest <- inputLoop
      return (line : rest)

topN :: (Ord as) => Int -> [as] -> [as]
topN n l = take n $ reverse $ sort l

main = do
  a <- inputLoop
  let b = split null a
  let nums = filter (/= []) (map (map read) b :: [[Int]])
  let groupSums = map sum nums
  let ans = sum $ topN 3 groupSums
  print ans
