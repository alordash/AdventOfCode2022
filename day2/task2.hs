import           Data.Bifunctor (Bifunctor (bimap))

data InputEnemyTurn
  = A
  | B
  | C
  deriving (Show, Read, Eq)

data InputOutcome
  = X
  | Y
  | Z
  deriving (Show, Read, Eq)

data Turn
  = Rock
  | Paper
  | Scissors
  deriving (Show, Read, Eq)

data Outcome
  = Lose
  | Draw
  | Win
  deriving (Show, Read, Eq)

toTurn :: InputEnemyTurn -> Turn
toTurn x =
  case x of
    A -> Rock
    B -> Paper
    C -> Scissors

toOutcome :: InputOutcome -> Outcome
toOutcome x =
  case x of
    X -> Lose
    Y -> Draw
    Z -> Win

type Strategy = (Turn, Turn)

inputLoop :: IO [String]
inputLoop = do
  line <- getLine
  if line == "."
    then return []
    else do
      rest <- inputLoop
      return (line : rest)

calcTurn :: (Turn, Outcome) -> Turn
calcTurn x =
  case x of
    (Rock, Lose)     -> Scissors
    (Paper, Lose)    -> Rock
    (Scissors, Lose) -> Paper
    (any, Draw)      -> any
    (Rock, Win)      -> Paper
    (Paper, Win)     -> Scissors
    (Scissors, Win)  -> Rock

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
  let setups =
        map ((\[x, y] -> (toTurn (read x), toOutcome (read y))) . words) lines
  let strategies = map (\(t, o) -> (t, calcTurn (t, o))) setups
  let scores = map calcStrategyScore strategies
  print strategies
  print scores
  let total = sum scores
  print total
