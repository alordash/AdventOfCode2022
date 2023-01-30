import           Data.List (isPrefixOf)

inputLoop :: IO [String]
inputLoop = do
  line <- getLine
  if line == "."
    then return []
    else do
      rest <- inputLoop
      return (line : rest)

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

extractFilesFromContents :: [String] -> [(String, Int)]
extractFilesFromContents [] = []
extractFilesFromContents (x:xs)
  | "dir " `isPrefixOf` x = extractFilesFromContents xs
  | otherwise = file : extractFilesFromContents xs
  where
    file = (name, size)
    name = xWords !! 1
    size = read $ head xWords
    xWords = words x

data Path =
  Path
    { name :: String
    , size :: Int
    , path :: [String]
    }
  deriving (Show, Read, Eq)

pathParents :: Path -> [String]
pathParents Path {name = _, size = _, path = parents} = parents

moveOutDir = ".."

processGetFiles :: [String] -> [Command] -> [Path]
processGetFiles _ [] = []
processGetFiles currentPath (c:cs) =
  case c of
    Cd subdirName -> processGetFiles newPath cs
      where newPath =
              if subdirName == moveOutDir
                then drop 1 currentPath
                else subdirName : currentPath
    Ls contents -> lsPaths ++ processGetFiles currentPath cs
      where lsPaths =
              map
                (\(name, size) ->
                   Path {name = name, size = size, path = currentPath})
                lsFiles
            lsFiles = extractFilesFromContents contents

getFiles :: [Command] -> [Path]
getFiles [] = []
getFiles commands =
  map
    (\Path {name = name, size = size, path = path} ->
       Path {name = name, size = size, path = reverse path}) $
  processGetFiles [] (drop 1 commands)

data FS
  = File String Int
  | Dir String [FS]
  deriving (Show, Read, Eq)

testFS =
  Dir
    "/"
    [ Dir
        "a"
        [ Dir "e" [File "i" 584]
        , File "f" 29116
        , File "g" 2557
        , File "h.lst" 62596
        ]
    , File "b.txt" 14848514
    , File "c.dat" 8504156
    , Dir
        "d"
        [ File "j" 4060174
        , File "d.log" 8033020
        , File "d.ext" 5626152
        , File "k" 7214296
        ]
    ]

tryExtractFS :: [FS] -> String -> (Maybe FS, [FS])
tryExtractFS [] _ = (Nothing, [])
tryExtractFS ((File name size):ds) searchDirName =
  if name == searchDirName
    then (Just $ File name size, ds)
    else case tryExtractFS ds searchDirName of
           (Just a, rest) -> (Just a, File name size : rest)
           (Nothing, _)   -> (Nothing, [])
tryExtractFS ((Dir name cs):ds) searchDirName =
  if name == searchDirName
    then (Just $ Dir name cs, ds)
    else case tryExtractFS ds searchDirName of
           (Just a, rest) -> (Just a, Dir name cs : rest)
           (Nothing, _)   -> (Nothing, [])

addPathToFS :: FS -> Path -> FS
addPathToFS (File name size) _ = File name size
addPathToFS (Dir name contents) Path { name = filename
                                     , size = filesize
                                     , path = []
                                     } =
  Dir name (File filename filesize : contents)
addPathToFS (Dir name contents) Path { name = filename
                                     , size = filesize
                                     , path = (d:ds)
                                     } =
  if null ds
    then Dir
           name
           (Dir subdirName (File filename filesize : subdirContents) :
            restContent)
    else Dir
           name
           (addPathToFS
              (Dir subdirName subdirContents)
              Path {name = filename, size = filesize, path = ds} :
            restContent)
  where
    maybeSubdir = tryExtractFS contents d
    (Dir subdirName subdirContents, restContent) =
      case maybeSubdir of
        (Just a, rest) -> (a, rest)
        (Nothing, _)   -> (Dir d [], contents)

processFormFS :: FS -> [Path] -> FS
processFormFS = foldl addPathToFS

-- processFormFS fs (p:ps) = case tryFindFS [fs] (pathName p) of
formFS :: [Path] -> FS
formFS [] = Dir "/" []
formFS ps = processFormFS (Dir "/" []) ps

calcSize :: FS -> Int
calcSize (File _ size)    = size
calcSize (Dir _ contents) = sum $ map calcSize contents

mapFS :: (FS -> a) -> [FS] -> [a]
mapFS _ [] = []
mapFS fmap (f:fs) =
  (case f of
     File name size    -> []
     Dir name contents -> fmap (Dir name contents) : mapFS fmap contents) ++
  mapFS fmap fs

calcFSSizes :: [FS] -> [Int]
calcFSSizes [] = []
calcFSSizes fs = mapFS calcSize fs

sizeThreshold = 100000

main = do
  lines <- inputLoop
  let commands = parseCommands lines
  let files = getFiles commands
  let fs = formFS files
  let sizes = calcFSSizes [fs]
  print fs
  print sizes
  let fitSizes = filter (<= sizeThreshold) sizes
  let answer = sum fitSizes
  print answer
