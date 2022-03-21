import Data.Char
import Assn2b

gcdMine :: Integral a => a -> a -> a 
gcdMine x y
    | y <= 0 = x
    | otherwise = gcdMine y (mod y x)

gcdCheck :: Integral a => a -> a -> (a, a, String)
gcdCheck x y
    | otherwise = (myAnswer, correctAnswer, comment)
    where myAnswer = gcdMine x y
          correctAnswer = gcd x y
          comment = if myAnswer == correctAnswer then "Matches" else "Does not Match"

fibNum :: Int -> Int
fibNum n 
    | n <= 0    = n
    | n == 1     = n
    | otherwise = fib !! n
    where fib = 0 : 1 : zipWith (+) fib (tail fib)

fibList :: Int -> [Int]
fibList n 
    | n <= 0    = [0]
    | otherwise = (fibNum(n) : fibList(n-1))
    
fibSeq :: Int -> [Int]
fibSeq x = reverse(fibList x)

count :: (Eq a, Num b) => a -> [a] -> b
count a [] = 0
count a (b:bs)
    | a == b    = 1 + count a bs
    | otherwise = count a bs

sanitize :: [Char] -> [Char]
sanitize a
    | a == []       = []
    | head a == ' ' = "%20" ++ sanitize (tail a)
    | otherwise     = head a:sanitize (tail a)

multListBy10 :: [Int] -> [Int]
multListBy10 bs = map (10 * ) bs

incrementList1 :: [Int] -> [Int]
incrementList1 bs = map (1 + ) bs

incrementList2 :: String -> String
incrementList2 as = map (chr.(+1).ord) as

containsDivisibleBy42 :: [Int] -> Bool
containsDivisibleBy42 = any ((0==).(`mod` 42))

powersOf10 :: [Int] -> [Int]
powersOf10 xs = zipWith (^) [10,10..] xs

stringStripper :: String -> String
stringStripper = xs . xs
    where xs = reverse . dropWhile (==' ')

evenNumbers :: [Int] -> Bool
evenNumbers = all ((0==).(`mod` 2))

addNotToList :: [String] -> [String]
addNotToList xs = map ("not "++ ) xs

reverseListString :: [String] -> [String]
reverseListString xs = map (reverse ) xs

addNumbers :: (Num a) => a -> a -> a
addNumbers = \a -> \b -> a + b

times4 :: (Num a) => a -> a
times4 = \a -> a * 4

secondElement :: [Int] -> Int
secondElement = \a -> a !! 1

roundSquare :: Int -> Int
roundSquare = \a -> floor (sqrt (fromIntegral a))

splitSentence :: String -> [String]
splitSentence = \a -> words (a)

triangles :: [(Float, Float)] -> [(Float, Float, Float)]
triangles = map (\(a,b) -> (a, b, sqrt(a^2+b^2))) 

main=do
--Problem 1
putStrLn "-Problem 1-"
putStrLn "Input: 18, 42"
putStr "Output: "
print(gcdMine 18 42)
putStrLn "gcdCheck... check"
print(gcdCheck 111 259)
print(gcdCheck 2945 123042) 
print(gcdCheck (2*5*7)(7*23))
putStrLn""

--Problem 2
putStrLn "-Problem 2-"
putStrLn "Input: 20"
putStr "Output: "
print(fibSeq 20)
putStrLn ""

--Problem 3
putStrLn "-Problem 3-"
putStrLn "Input1: 7 [1,7,6,2,7,7,9]"
putStr "Output1: "
print(count 7 [1,7,6,2,7,7,9])
putStrLn "Input2: 'w' \"western oregon wolves\""
putStr "Output2: "
print(count 'w' "western oregon wolves")
putStrLn ""
            
--Problem 4
putStrLn "-Problem 4-"
putStrLn "Input1: \"http://wou.edu/my homepage/I love spaces.html\""
putStr "Output1: "
print(sanitize "http://wou.edu/my homepage/I love spaces.html")
putStrLn ""

--Problem 5
putStrLn "-Problem 5-"
putStrLn "Input: [1,2,3,4,5,6,7,8,9]"
putStr "Output: "
print(multListBy10 [1..9])
putStrLn ""
       
--Problem 6
putStrLn "-Problem 6-"
putStrLn "First Function"
putStrLn "Input: [1,2,3,4,5,6,7,8,9]"
putStr "Output: "
print(incrementList1 [1..9])
putStrLn "Second Function"
putStrLn "Input: \"Haskell is fun.\""
putStr "Output: "
print(incrementList2 "Haskell is fun.")
putStrLn ""

--Problem 7
putStrLn "-Problem 7-"
putStrLn "Input1: [12,15,87,968,54,23,5875,665,25,2]"
putStr "Output1: "
print(containsDivisibleBy42 [12,15,87,968,54,23,5875,665,25,2])
putStrLn "Input1: [987,85,53,336,2125,21,5151,84,646,64]"
putStr "Output1: "
print(containsDivisibleBy42 [987,85,53,336,2125,21,5151,84,646,64])
putStrLn ""
      
--Problem 8
putStrLn "-Problem 8-"
putStrLn "Input1: [1,3,5,7,9]"
putStr "Output1: "
print(powersOf10 [1,3,5,7,9])
putStrLn ""
      
--Problem 9
putStrLn "-Problem 9-"
putStrLn "Input: \"That is pretty neat         \""
putStr "Output: "
print(stringStripper "That is pretty neat         ")
putStrLn ""
           
--Problem 10
putStrLn "-Problem 10-"
putStrLn "Input: [2,2,4,6]"
putStr "Output: "
print(evenNumbers [2,2,4,6])
putStrLn ""
      
--Problem 11
putStrLn "-Problem 11-"
putStrLn "Input: [\"my chair\",\"my problem\"]"
putStr "Output: "
print(addNotToList ["my chair", "my problem"])
putStrLn ""

--Problem 12
putStrLn "-Problem 12-"
putStrLn "Input: [\"This\",\"is\",\"a\",\"sentence\"]"
putStr "Output: "
print(reverseListString ["This","is","a","sentence"])
putStrLn ""
       
--Problem 13
putStrLn "-Problem 13-"
putStrLn "Input: 8 9"
putStr "Output: "
print(addNumbers 8 9)
putStrLn ""

--Problem 14
putStrLn "-Problem 14-"
putStrLn "Input: 8"
putStr "Output: "
print(times4 8)
putStrLn ""

--Problem 15
putStrLn "-Problem 15-"
putStrLn "Input: [1,4,6,7,8,9]"
putStr "Output: "
print(secondElement [1,4,6,7,8,9])
putStrLn ""
   
--Problem 16
putStrLn "-Problem 16-"
putStrLn "Input: 17"
putStr "Output: "
print(roundSquare 17)
putStrLn ""
      
--Problem 17
putStrLn "-Problem 17-"
putStrLn "Input: \"This is a sentence written in the usual way\""
putStr "Output: "
print(splitSentence "This is a sentence written in the usual way")
putStrLn ""

--Problem 18
putStrLn "-Problem 18-"
putStrLn "Input: [(3,4),(5,16),(9.4,2)]"
putStr "Output: "
print(triangles [(3,4),(5,16),(9.4,2)])
putStrLn ""
       
--Part02_Problem 1
putStrLn "-Part02_Problem 1-"
putStrLn "In comments in source code"
putStrLn ""
       
--Part02_Problem 2
putStrLn "-Part02_Problem 2-"
putStrLn "Input: [9,8,7,6,5,4,3,2,1,0]"
putStr "Output: "
print(myLength [9,8,7,6,5,4,3,2,1,0])
putStrLn ""

--Part02_Problem 3.1
putStrLn "-Part02_Problem 3.1-"
putStrLn "Input: [1,2,3,4,5]"
putStr "Output: "
print(convertInttoStringLeft [1,2,3,4,5])
putStrLn ""
      
--Part02_Problem 3.2
putStrLn "-Part02_Problem 3.2-"
putStrLn "Input: [1,2,3,4,5]"
putStr "Output: "
print(convertInttoStringRight [1,2,3,4,5])
putStrLn ""
        
--Part02_Problem 4.1
putStrLn "-Part02_Problem 4.1"
putStrLn "Original Function: length (filter (<20) [1..100])"
putStr "Output: "
print(length (filter (<20) [1..100]))
putStrLn "Rewritten Function: length $ filter (<20) [1..100]"
putStr "Output: "
print(length $ filter (<20) [1..100])
putStrLn ""
        
--Part02_Problem 4.2
putStrLn "-Part02_Problem 4.2"
putStrLn "Original Function: take 10 (cycle (filter (>5) (map (*2) [1..10])))"
putStr "Output: "
print(take 10 (cycle (filter (>5) (map (*2) [1..10]))))
putStrLn "Rewritten Function: take 10 . cycle . filter (>5) . map (*2) [1..10]"
putStr "Output: "
print(take 10 . cycle . filter (>5) $ map (*2) [1..10])
putStrLn ""
