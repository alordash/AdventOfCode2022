import           Data.List (isPrefixOf)

inputLoop :: IO [String]
inputLoop = do
  line <- getLine
  if line == "."
    then return []
    else do
      rest <- inputLoop
      return (line : rest)

data Path
  = Dir
      { name     :: String
      , contents :: [Path]
      }
  | File
      { name :: String
      , size :: Int
      }
  deriving (Show, Read, Eq)

data Command
  = Cd String
  | Ls [String]
  deriving (Show, Read, Eq)

parseCommands :: [String] -> [Command]
parseCommands [] = []
parseCommands (line:rest)
  | "$ cd" `isPrefixOf` line = Cd (lineWords !! 2) : parseCommands rest
  | "$ ls" `isPrefixOf` line = Ls lsStrs : parseCommands lsRest
  where
    lineWords = words line
    (lsStrs, lsRest) = processLs rest
    processLs :: [String] -> ([String], [String])
    processLs [] = ([], [])
    processLs (line':rest')
      | "$" `isPrefixOf` line' = ([], line' : rest')
      | otherwise = (line' : lsStrs', lsRest')
      where
        (lsStrs', lsRest') = processLs rest'

main = do
  lines <- inputLoop
  let commands = parseCommands lines
  print commands
