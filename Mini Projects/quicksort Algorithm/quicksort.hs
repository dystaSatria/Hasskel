
quicksort :: Ord b => [b] -> [b]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

main :: IO ()
main = do
    let myList = [4, 7, 2, 9, 1, 5, 3, 8, 6]
    putStrLn "Unsorted list:"
    print myList
    putStrLn "Sorted list:"
    print (quicksort myList)
