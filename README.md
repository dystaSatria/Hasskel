# Haskel Summary

* Hello World
```hs
main = do 
  putStrLn "Hello, World!"
```
* With Do
```hs
main = do
    putStrLn "Enter a number:"         -- Statement?
    str <- getLine                     -- Statement?
    putStrLn ("You entered: " ++ str)  -- Statement?

```


* Without Do
```hs
main =
    putStrLn "Enter a number:" >>= (\_   ->  -- Expression
     getLine                   >>= (\str ->  -- Sub-expression
      putStrLn ("You entered: " ++ str) ))   -- Sub-expression

```

* Arithmatic operation

```hs
main = do
    print(3 + 2)
    print(19 - 27)
    print(2.35*8.6)
    print(8.7/3.1)
    print(5^3)
    print(mod 19 3)
    print(19 `mod` 3)
    print((-3)*(-7))
```
Output: 
```output
5
-8
20.21
2.8064516129032255
125
1
1
21
```

* Comparison Operator

```hs
main = do
    print(5 == 5)
    print('a' /= 'b')
    print("Dicoding" < "Indonesia")
    print(5 <= 5)
    print('b' > 'a')
    print("Indonesia" >= "Dicoding")
```

Output :

```output
True
True
True
True
True
True
```

* Logical Operators

```hs
main = do
    print(True && True)
    print(True && False)
    print(True || False)
    print(not False)
    print(not (True && True))
    print(not (True || False))

```

Output : 

```output
True
False
True
True
False
False

```

```hs
main = do
    -- AND operator
    print((10 < 15) && (20 > 10)) {- True && True -> True -}
    print((5 > 3) && ('p' >= 'q')) {- True && False -> False -}  
    
    -- OR operator
    print((10 < 15) || (20 > 10)) {- True || True -> True -}
    print((5 > 3) || ('p' >= 'q')) {- True || False -> True -}

    -- NOT operator
    print(not ((10 < 15) && (20 > 10))) {- not (True && True) -> not True -> False -}
    print(not ((5 > 3) && ('p' >= 'q'))) {- not (True && False) -> not False -> True -}

```
