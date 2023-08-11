import Data.Char (toUpper)
import Data.Time.Clock.POSIX
import System.IO

data Move = Rock | Paper | Scissors deriving (Show, Eq)

beats :: Move -> Move -> Bool
beats Rock Scissors = True
beats Paper Rock = True
beats Scissors Paper = True
beats _ _ = False

getComputerMove :: IO Move
getComputerMove = do
    currentTime <- getPOSIXTime
    let seed = round currentTime :: Int
        randomValue = seed `mod` 3
    return $ case randomValue of
        0 -> Rock
        1 -> Paper
        _ -> Scissors

main :: IO ()
main = do
    putStrLn "Rock Paper Scissor Game | Haskell"
    hSetBuffering stdout NoBuffering
    putStrLn "Enter your move (rock/paper/scissors):"
    input <- fmap (map toUpper) getLine
    let playerMove = case input of
                        "ROCK" -> Rock
                        "PAPER" -> Paper
                        "SCISSORS" -> Scissors
                        _ -> error "Invalid move"
    
    computerMove <- getComputerMove
    
    putStrLn $ "Computer's move: " ++ show computerMove
    putStrLn $ "Your move: " ++ show playerMove
    
    if playerMove `beats` computerMove
        then putStrLn "You win!"
        else if computerMove `beats` playerMove
            then putStrLn "Computer wins!"
            else putStrLn "It's a draw!"
