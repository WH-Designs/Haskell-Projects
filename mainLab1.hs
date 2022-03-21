--squareRoot :: Int -> Int
squareRoot x = sqrt x

--asciiPrevious :: char -> char
asciiPrevious char = pred char

--verifyEven :: Int -> Bool
verifyEven x = mod (x * 3 + 1) 2 == 0

--gaussianProduct :: Int -> Int
gaussianProduct x = product [1,3..100]

--largestInList :: [Int] -> Int
firstRem x = tail (lastRem x)
lastRem x = init x
largestInList x = maximum(firstRem x)

--constructList :: [] -> [Int]
constructList = 2 : 32 : 12 : (-12) : []

--firstXEvens :: Int -> Int
firstXEvens x = take x [2,4..] 

--oddsDivisible3and7 :: Int -> [Int]
oddsDivisible3and7 x = [x | x <- [1..200],
 x `mod` 3 == 0 && x `mod` 7 == 0]
 
--oddsDivisible9 :: Int -> [Int]
oddsDivisible9 x = [x | x <- [100..200], x `mod` 9 == 0]

--countNegs :: [Int] -> Int
countNegs n = length [ x | x <- n, x < 0 ]

--hexMaps :: [(Int, Char)]
hexMaps = zip [0..15] ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']

--makeList :: Int -> [[Int]]
makeList n = [ [1..x] | x <- [1..n] ]

sanitize :: [Char] -> [Char]
sanitize st = [ c | c <- st, c <- if(c == ' ') then "%20" else [c]] 

getSuit :: Int -> String
getSuit 0 = "Heart"
getSuit 1 = "Diamond"
getSuit 2 = "Spade"
getSuit 3 = "Club"
getSuit x = "Not in suit"

dotProduct :: (Double,Double,Double) -> (Double,Double,Double) -> Double
dotProduct (a1, b1, c1) (a2, b2, c2) = ((a1 * a2) + (b1 * b2) + (c1 * c2))

reverseFirstThree :: [a] -> [a]
reverseFirstThree [] = []
reverseFirstThree [x] = [x]
reverseFirstThree (x:y:[]) = [y,x]
reverseFirstThree (x:y:z:zs) = z:y:x:zs

feelsLike :: Double -> String
feelsLike temp
    | temp < -44.3   = "frostbite central!"
    | temp < 32.0    = "Freezing"
    | temp < 60.0    = "Cool"
    | temp < 80.0    = "Warm"
    | otherwise      = "Way Too Hot"

feelsLike2 :: Double -> String
feelsLike2 temp
    | celtemp < 30.2   = "Freezing"
    | celtemp < 50.0   = "Cold"
    | celtemp < 100    = "Hot"
    | celtemp > 212.0  = "Oven-like"
    | otherwise        = "Way Too Hot"
    where celtemp = (temp / 1.8 - 32.0)

cylinderToVolume :: [(Double,Double)] -> [Double]
cylinderToVolume l = [a+b | (a,b) <- l]

main=do
--problem 1
putStrLn("")
putStrLn("-Problem 1-")
putStrLn("Input: 818281336460929553769504384519009121840452831049")
putStr("Output: ")
print(squareRoot 818281336460929553769504384519009121840452831049)
putStrLn("")

--Problem 2
putStrLn("-Problem 2-")
putStrLn("Input: 'A'")
putStr("Output: ")
print(asciiPrevious 'A')
putStrLn("")

--Problem 3
putStrLn("-Problem 3-")
putStrLn("Input1: 5")
putStr("Output1: ")
print(verifyEven 5)
putStrLn("Input2: 10")
putStr("Output2: ")
print(verifyEven 10)
putStrLn("Input3: 6541562")
putStr("Output3: ")
print(verifyEven 6541562)
putStrLn("")

--Problem 4
putStrLn("-Problem 4-")
putStrLn("Input: 100")
putStr("Output: ")
print(gaussianProduct 100)
putStrLn("")

--Problem 5
putStrLn("-Problem 5-")
putStrLn("Input: [99,23,4,2,67,82,49,-40]")
putStr("Output: ")
print(largestInList [99,23,4,2,67,82,49,-40])
putStrLn("")

--Problem 6
putStrLn("-Problem 6-")
putStr("Output: ")
print(constructList)
putStrLn("")

--Problem 7
putStrLn("-Problem 7-")
putStrLn("Input: 27")
putStr("Output: ")
print(firstXEvens 27)
putStrLn("")

--Problem 8
putStrLn("-Problem 8-")
putStrLn("Input: 200")
putStr("Output: ")
print(oddsDivisible3and7 200)
putStrLn("")

--Problem 9
putStrLn("-Problem 9-")
putStrLn("Input: 200")
putStr("Output: ")
print(oddsDivisible9 200)
putStrLn("")

--Problem 10
putStrLn("-Problem 10-")
putStrLn("Input: [(-4),6,7,8,(-14)]")
putStr("Output: ")
print(countNegs [(-4),6,7,8,(-14)])
putStrLn("")

--Problem 11
putStrLn("-Problem 11-")
putStr("Output: ")
print(hexMaps)
putStrLn("")

--Problem 12
putStrLn("-Problem 12-")
putStrLn("Input1: 7")
putStr("Output1: ")
print(makeList 7)
putStrLn("Input2: 0")
putStr("Output2: ")
print(makeList 0)
putStrLn("Input3: -1")
putStr("Output3: ")
print(makeList (-1))
putStrLn("")

--Problem 13
putStrLn("-Problem 13-")
putStrLn("Input: \"http://wou.edu/my homepage/I love spaces.html\"")
putStr("Output: ")
print(sanitize "http://wou.edu/my homepage/I love spaces.html")
putStrLn("")

--Problem 14
{-
take :: Int -> [a] -> [a]
Int
Rational
Ratio Int
Integer

succ :: Enum a => a -> a
Int
Float
Double
Integer

min :: Ord a => a -> a -> a
Ratio Int
Rational
-}

--Problem 15
putStrLn("-Problem 15-")
putStrLn("Input1: 0")
putStr("Output1: ")
print(getSuit 0)
putStrLn("Input2: 1")
putStr("Output2: ")
print(getSuit 1)
putStrLn("Input3: 2")
putStr("Output3: ")
print(getSuit 2)
putStrLn("Input4: 3")
putStr("Output4: ")
print(getSuit 3)
putStrLn("Input5: 77")
putStr("Output5: ")
print(getSuit 77)
putStrLn("")

--Problem 16
putStrLn("-Problem 16-")
putStrLn("Input: (1,2,3.0) (4.0,5,6)")
putStr("Output: ")
print(dotProduct (1,2,3.0) (4.0,5,6))
putStrLn("")

--Problem 17
putStrLn("-Problem 17-")
putStrLn("Input1: [1]")
putStr("Output1: ")
print(reverseFirstThree [1])
putStrLn("Input2: [1,2]")
putStr("Output2: ")
print(reverseFirstThree [1,2])
putStrLn("Input3: [1,2,3]")
putStr("Output3: ")
print(reverseFirstThree [1,2,3])
putStrLn("Input4: [1,2,3,4]")
putStr("Output4: ")
print(reverseFirstThree [1,2,3,4])
putStrLn("")

--Problem 18
putStrLn("-Problem 18-")
putStrLn("Input1: -200")
putStr("Output1: ")
print(feelsLike (-200))
putStrLn("Input2: 200")
putStr("Output2: ")
print(feelsLike 200)
putStrLn("Input3: -45.3")
putStr("Output3: ")
print(feelsLike (-45.3))
putStrLn("Input4: 79")
putStr("Output4: ")
print(feelsLike 79)
putStrLn("")

--Problem 19
putStrLn("-Problem 19-")
putStrLn("Input1: -200")
putStr("Output1: ")
print(feelsLike2 (-200))
putStrLn("Input2: -0.1")
putStr("Output2: ")
print(feelsLike2 (-0.1))
putStrLn("Input3: -42.9444444444444444444444444445")
putStr("Output3: ")
print(feelsLike2 (-42.9444444444444444444444444445))
putStrLn("Input4: 100")
putStr("Output4: ")
print(feelsLike2 100)
putStrLn("")

--Problem 20
putStrLn("-Problem 20-")
putStrLn("Input: [(2,5.3),(4.2,9),(1,1),(100.394)]")
putStr("Output: ")
print(cylinderToVolume [(2,5.3),(4.2,9),(1,1),(100.3,94)])
