main :: IO ()
main = do putStrLn "What is 2 + 2?"
          x <- readLn
          if x == 4
              then putStrLn "You're right!"
              else putStrLn "You're wrong!"

main2 :: IO ()
main2 = do
        print(2*2)
        print((*) 2 2)
