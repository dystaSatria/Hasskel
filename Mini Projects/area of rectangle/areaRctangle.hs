main :: IO ()
main = do
    putStrLn "Enter length: "
    inputLength <- getLine
    putStrLn "Enter width: "
    inputWidth <- getLine

    let length = read inputLength :: Double
        width = read inputWidth :: Double
        area = length * width

    putStrLn ("The area of the rectangle is: " ++ show area)
