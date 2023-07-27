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

```hs
n = 5
m = n + 1

main = print(m)
```
Output :
```output
6
```

```hs
inc n = n + 1

main = print(inc 5)
```
Output :
```output
6
```

* / Operator

```hs

i = 30 :: Int

main = print (i / i)
```
Output : ERROR
Just can be work on float

True code :

```hs 
i = 30 :: Int
j = 5 :: Int

main = do
    print (div i j)
    print (i `div` j)

```

```output
6
6
```

### * Data Types

* Integer

```hs
n :: Integer
n = 1234567890987654321987340982334987349872349874534

sangatBesar :: Integer
sangatBesar = 2^(2^(2^(2^2)))

main = do
    print(n)
    print(sangatBesar)
```

* Int

```hs
usia :: Int
usia = 25

luasBangunan :: Int
luasBangunan = 200000

gajiKaryawan :: Int
gajiKaryawan = 8500000

main = print(usia, luasBangunan, gajiKaryawan)
```

* Float
<br>

7 digit
```hs
massaTubuh, nilaiPi :: Float

massaTubuh = 61.5
nilaiPi = 3.141592653589793238462643383279502884197

main = print(massaTubuh, nilaiPi)
```

* Double
<br>

15 digit
  
```hs
nilaiPi :: Double
nilaiPi = 3.141592653589793238462643383279502884197

main = print(nilaiPi)
```

* Char

```hs
skor, karakter, simbol, ascii :: Char

skor = 'A'
karakter = 'q'
simbol = '!'
ascii = '\67'

main = print(skor, karakter, simbol, ascii)

```

```output
('A','q','!','C')
```

* String

```hs
perusahaan, alamat, industri :: String

perusahaan = "Dicoding Indonesia"
alamat = "Jl. Batik Kumeli 50 Bandung"
industri = "Teknologi"

main = print(perusahaan, alamat, industri)
```

```output
("Dicoding Indonesia","Jl. Batik Kumeli 50 Bandung","Teknologi")
```


* Bool

```hs
x, y :: Bool

x = True
y = False

main = print(x == y)
```

```output
False
```

* ### Structure Data
```
  </t>* List => [1,2,3]
  </t>* Tuple => (1,2,3)
```
* Operator Konkatenasi

```hs
main = do
    print([1, 2, 3, 4] ++ [5, 6, 7, 8])
    print("Dicoding" ++ " " ++ "Indonesia")
    print(['H', 'a', 's'] ++ ['k', 'e', 'l', 'l'] )

```
Output : 

```output
[1,2,3,4,5,6,7,8]
"Dicoding Indonesia"
"Haskell"
```

* Operator Cons

 ```hs
main = do
    print('A':" SMALL CAT")
    print(1:[2,3,4,5])

```
Output :

```output
"A SMALL CAT"
[1,2,3,4,5]

```

* Comparison Operator

```hs
main = do
    print([3,2,1] > [2,10,100])
    print([3,2,1] >= [3,2,2])
    print([2,4,2] < [3,4])
    print([3,4,2] <= [3,4])
    print([3,4,2] == [3,4,2])
    print([3,2,1] == [3,2,6])

```

Output :

```output
True
False
True
False
True
False

```
* Range Operator

```hs
main = do 
    print([1..(-930)])  
    print(['a'..'z'])
    print(['K'..'Z'])
    print([1..20])  
```
Output :

```output
[]
"abcdefghijklmnopqrstuvwxyz"
"KLMNOPQRSTUVWXYZ"
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

```

```hs
main = do 
    print([-2,4..20])
    print([3,6..20])

```

Output :

```output
[-2,4,10,16]
[3,6,9,12,15,18]

```
