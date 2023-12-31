# Haskell: Operator, Expressions, and Variables

In programming, the variations between programming languages can be substantial, depending on their designs. Languages like Haskell exhibit unique characteristics regarding operators, expressions, and variables:

## Operators and Expressions

1. **Purely Functional**: Haskell adopts pure functional programming, where all functions are pure, devoid of side effects, and consistently yield values for the same input.
2. **Pattern Matching**: Utilization of patterns enables matching data structures in function definitions, enhancing code clarity.
3. **Lazy Evaluation**: Haskell uses lazy evaluation, allowing the utilization of infinite data structures.
4. **Operator Precedence**: Haskell has strict rules governing the order of operations, with the ability to set operator priorities to avoid ambiguity.

## Variable Concept

In Haskell, variables are immutable. Once assigned a value, they cannot be altered. Altering the value of a variable effectively creates a new variable with the updated value.

## Insights

1. Operators in Haskell, like in other languages, are symbols or functions used for mathematical operations and data manipulation, with unique rules in each language.
2. Expressions in Haskell encompass combinations of code that yield values, ranging from simple mathematical operations to more intricate constructs.
3. Variables in Haskell are immutable, aligning with the principles of functional programming.

Every programming language possesses its unique advantages based on its design and intended usage. Selecting the appropriate language is contingent upon the project context and requirements.

---

## Difference between Expressions and Statements

### Expressions

Expressions are code segments or combinations that yield values, such as numbers, strings, booleans, or the outcomes of mathematical operations.

### Statements

Statements are code snippets that execute specific actions or instructions, potentially inducing side effects and not always yielding a value.

In summary:
- Expressions yield values.
- Statements execute actions or instructions.

Note that these definitions might slightly deviate across languages. Haskell typically maintains a clearer distinction between pure expressions and statements.

---

## Example Expression

Given the expression:

```haskell
"Dicoding" == ['D','I','C','O','D','I','N','G']


## Start in PlayGround

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

* Comment

```hs
main :: IO () -- mendeklarasikan tipe data dari main
{-
  Kode di bawah ini akan mencetak
  teks "Hello, World!" ke layar.
-}
main = putStrLn "Hello, World!"

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
    print([20,19..1])

```

Output :

```output
[-2,4,10,16]
[3,6,9,12,15,18]
[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]

```

* Reverse

```hs
main = print(reverse [1,2,3,4,5])

```

Output : 

```output
5,4,3,2,1
```

* Head | Tail | Last | Init

```hs
main = do
    print(head [5,4,3,2,1])
    print(tail [5,4,3,2,1])
    print(last [5,4,3,2,1])
    print(init [5,4,3,2,1])

```

Output :


```output
5
[4,3,2,1]
1
[5,4,3,2]

```

* Take | Drop | sum | length

```hs
  print(length [5,4,3,2,1]) -- 5
  print(null [1,2,3]) --False
  print(null []) --True
  print(take 3 [5,4,3,2,1]) --5 4 3
  print(drop 3 [8,4,2,8,8,9]) --8 8 9
  print(drop 0 [1,2,3,4]) -- [1,2,3,4]
  print(drop 100 [1,2,3,4]) --[]
  print(product [6,2,1,2]) -- 6*2*1*2
  print(product [1,10,3,5,12,0]) --0

```

* List in list

```hs
x = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]] 

main = do
    print(x) 
    print(x ++ [[1,1,1,1]])
    print([6,6,6]:x)
    print(x !! 2)

```

Output :

```output
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3],[1,1,1,1]]
[[6,6,6],[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
[1,2,2,3,4]


```

* Tuple

```hs
main = do
    print((1,'a'))
    print((8,11),(4,5))
    print(("Christopher", "Walken", 55))
    print(((2,3), [2,3]))
    print([(1,2),(8,11),(4,5)])
```



```hs

main = do
    print(([1,2],[3,4])) -- [1,2] ,[3,4]
    print((['a','b','c']), (['d','e'])) -- ("abc","de")

```

* FST

```hs
main = do
    print(fst (12,42))
    print(fst ("Wow", False))

```

```hs
main = do
    print((1,2) == (8,11)) --False
    print((5,75,13) > (15,5,33)) --False
    print((1,2) < (3,4)) --True

```

* SND

```hs
main = do
    print(snd (12,42))
    print(snd ("Wow", False))

```

* ZIP

```hs
main = do
    print(zip [19,2,3,4,5] [5,5,5,5,5]) --[(19,5),(2,5),(3,5),(4,5),(5,5)]
    print(zip [1 .. 5] ["one", "two", "three", "four", "five"]) --[(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]
    print(zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]) --[(5,"im"),(3,"a"),(2,"turtle")]
```

* Function and Parameter Naming

```hs
fungsiKuadrat x = x*x
duaKuadrat x = x*x
fungsiKuadrat’ x = x*x
fungsiKuadrat_ x = x*x
tambah5 x = x+5
ambil_2 list = take 2 list
```

<br>
<br>
<br>

## Start on The VS Code

  
* Download The extension in Haskell

* Open terminal
```console
ghci
```

* Next Step
```console
:load <YourProgram.hs>
```

* Next Step
```console
main
```
