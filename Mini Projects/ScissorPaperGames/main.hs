import System.Random

data Move = Rock | Paper | Scissors deriving (Show, Eq)

beats :: Move -> Move -> Bool
beats Rock Scissors = True
beats Paper Rock = True
beats Scissors Paper = True
beats _ _ = False

getPlayerMove :: IO Move
getPlayerMove = do
    putStrLn "Enter your move (rock/paper/scissors):"
    input <- getLine
    case input of
        "rock" -> return Rock
        "paper" -> return Paper
        "scissors" -> return Scissors
        _ -> do
            putStrLn "Invalid move, please try again."
            getPlayerMove

getComputerMove :: IO Move
getComputerMove = do
    randomValue <- randomRIO (0, 2) :: IO Int
    return $ case randomValue of
        0 -> Rock
        1 -> Paper
        _ -> Scissors

main :: IO ()
main = do
    putStrLn "Let's play Rock-Paper-Scissors!"
    playerMove <- getPlayerMove
    computerMove <- getComputerMove
    putStrLn $ "Computer's move: " ++ show computerMove
    putStrLn $ "Your move: " ++ show playerMove
    if playerMove `beats` computerMove
        then putStrLn "You win!"
        else if computerMove `beats` playerMove
            then putStrLn "Computer wins!"
            else putStrLn "It's a draw!"
