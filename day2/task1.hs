import           Data.Bifunctor (Bifunctor (bimap))

data InputEnemyTurn
  = A
  | B
  | C
  deriving (Show, Read, Eq)

data InputMyTurn
  = X
  | Y
  | Z
  deriving (Show, Read, Eq)

data Turn
  = Rock
  | Paper
  | Scissors
  deriving (Show, Read, Eq)

convertEnemyTurn :: InputEnemyTurn -> Turn
convertEnemyTurn x =
  case x of
    A -> Rock
    B -> Paper
    C -> Scissors

convertMyTurn :: InputMyTurn -> Turn
convertMyTurn x =
  case x of
    X -> Rock
    Y -> Paper
    Z -> Scissors

type Strategy = (Turn, Turn)

inputLoop :: IO [String]
inputLoop = do
  line <- getLine
  if line == "."
    then return []
    else do
      rest <- inputLoop
      return (line : rest)

calcTurnScore :: Turn -> Int
calcTurnScore t =
  case t of
    Rock     -> rockScore
    Paper    -> paperScore
    Scissors -> scissorsScore
  where
    rockScore = 1
    paperScore = 2
    scissorsScore = 3

calcRoundScore :: Strategy -> Int
calcRoundScore s =
  case s of
    (Rock, Scissors)     -> lose
    (Paper, Rock)        -> lose
    (Scissors, Paper)    -> lose
    (Rock, Rock)         -> draw
    (Paper, Paper)       -> draw
    (Scissors, Scissors) -> draw
    _                    -> win
  where
    lose = 0
    draw = 3
    win = 6

calcStrategyScore :: Strategy -> Int
calcStrategyScore (e, m) = calcTurnScore m + calcRoundScore (e, m)

main = do
  lines <- inputLoop
  let strategies =
        map
          ((\[x, y] ->
              ( convertEnemyTurn (read x :: InputEnemyTurn)
              , convertMyTurn (read y :: InputMyTurn))) .
           words)
          lines
  let scores = map calcStrategyScore strategies
  print strategies
  print scores
  let total = sum scores
  print total
