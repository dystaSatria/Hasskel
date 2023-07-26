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

* Concatenation Operator

```hs
main =do
   print([1, 2, 4] ++ [4, 5])
    print("Dicoding" ++ " Indonesia")
```

Output :

```output
1,2,4,4,5
"Dicoding Indonesia"

```

* If else

```hs
main = do
    print(if True then 5 else 3)
    print(if 5 > 3 then 5 else 3)
    print(if True then 5 else (if False then 3 else 5))
```
Output :

```output
5
5
5
```

```hs

main = do   
    let var = 26 
    
    if var == 0 
        then putStrLn "Number is zero" 
    else if var `mod` 2 == 0 
        then putStrLn "Number is Even" 
    else putStrLn "Number is Odd"

```
Output :

```output
Number is Even

```

* Fibonacci Example
```hs

fib n = do 
    if n == 0 then 1            
        else if n == 1 then 1
        else fib (n-1) + fib (n-2)

main = print(fib 8)

```

```output
34
```

* Variables Must Have Values
```hs
x = 10

main = print(x)
```
Error code :
```hs
x = 5
x = 10

main = print(x)
```
