import           Data.List (nub)

constChunkSize = 4

findFirstChunkOfUniqueChars :: Int -> String -> (Int, String)
findFirstChunkOfUniqueChars _ [] = (0, [])
findFirstChunkOfUniqueChars chunkSize s = processChunk 0 [] s
  where
    (current, rest) = splitAt (chunkSize - 1) s
    processChunk :: Int -> String -> String -> (Int, String)
    processChunk _ _ [] = (0, [])
    processChunk 0 _ (c:cs) = processChunk 1 [c] cs
    processChunk idx chunk (c:cs) =
      case length chunk of
        x
          | x < chunkSize -> processChunk (idx + 1) (c : chunk) cs
        _ ->
          if length (nub chunk) == length chunk
            then (idx, chunk)
            else processChunk (idx + 1) (c : init chunk) cs

main = do
  line <- getLine
  let firstChunkIdx = findFirstChunkOfUniqueChars constChunkSize line
  print firstChunkIdx
