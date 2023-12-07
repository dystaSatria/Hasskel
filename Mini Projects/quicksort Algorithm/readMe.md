# Quicksort Algorithm in Haskell

This repository contains an implementation of the Quicksort algorithm in Haskell.

## Algorithm Overview

The Quicksort algorithm is a sorting algorithm that utilizes a divide-and-conquer approach. It selects a pivot element from the list and partitions the remaining elements into two sublists: one with elements smaller or equal to the pivot, and the other with elements greater than the pivot. This process is applied recursively to each sublist until the entire list is sorted.

## Code Explanation

The function `quicksort` is defined as follows:

```haskell
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted


## Function Overview

The `quicksort` function takes a list of elements of type `a` that can be compared (`Ord a`) and returns a sorted list of elements.

- The base case `quicksort [] = []` handles an empty list, returning an empty list.

- The function recursively partitions the list by selecting a pivot element `x` and then creating two lists:
  - `smallerSorted`: Consists of elements less than or equal to the pivot.
  - `biggerSorted`: Consists of elements greater than the pivot.
  
  The result is obtained by concatenating the sorted `smallerSorted`, the pivot element `x`, and the sorted `biggerSorted`.

## How to Use

To utilize the Quicksort algorithm:

1. **Ensure you have the Glasgow Haskell Compiler (GHC) installed.**

2. **Clone or download this repository to your local machine.**

3. **Compile the `quicksort.hs` file using GHC:**

   Open a terminal or command prompt, navigate to the directory containing `quicksort.hs`, and run the following command:


4. **Run the compiled executable:**

After successful compilation, execute the compiled program by entering the following command:



This will execute the Quicksort algorithm on a sample list and display both the unsorted and sorted lists.

Feel free to modify the `quicksort` function or integrate it into your Haskell projects to efficiently sort lists of elements.
