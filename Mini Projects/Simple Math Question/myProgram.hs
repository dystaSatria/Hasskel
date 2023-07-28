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

main3 :: IO()
main3 = do
    print(succ 18) --output 19
    print(min 9 10) -- 9
    print(min 3.4 3.2) --3.2
    print(max 100 101 ) --101
    print(succ 9) + (max 5 4) + 1 --16 
