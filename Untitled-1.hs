import Text.ParserCombinators.ReadPrec (step)
import System.Win32 (xBUTTON1, COORD (yPos))


bmi :: Float -> Float -> String
bmi weight height 
  | n < 18.5 = "Underweight"
  | n < 24.9 = "Normal"
  | n < 29.9 = "Overweight"
  | otherwise = "Obese"
  where
    n = weight / (height * height)


fac :: Int -> Int
fac n = if n == 1 then 1 else n * fac (n-1)


sumTo :: Integer -> Integer
sumTo 1 = 1
sumTo n = n + sumTo(n-1) 


fib1 :: Num long => Int -> long
fib1 n = if n <= 1 then 1 else fib (n-1) + fib (n-2)


fibonacci :: Integer -> Integer
fibonacci = fibonacci' 0 1

fibonacci' :: Integer -> Integer -> Integer -> Integer
fibonacci' a b 1 = b
fibonacci' a b n = fibonacci' b (a+b) (n-1)

fib n = fibs !! n where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)


phi :: Double
phi = (sqrt 5 + 1) / 2


polynomial :: Double -> Double -> Double
polynomial x y = x^2 + 2*x*y + y^2


primes :: [Integer]
primes = [ n | n <- [2..] , all (\k -> n `mod` k /= 0) [2..n `div` 2] ]


collatz :: Integer -> Integer 
collatz 1 = 1
collatz n = if even n then collatz (div n 2) else collatz (3 * n + 1)


distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = sqrt (((x2-x1) ^ 2) + ((y2-y1) ^ 2))


postagePrice :: Int -> Int
postagePrice gr
  | gr <= 500 = 500
  | gr <= 5000 = 300 + gr
  | otherwise = 6000


concatanate :: [a] -> [a] -> [a]
concatanate xs ys = xs ++ ys 
 

findSmallest :: Ord a => [a] -> a
findSmallest [x] = x
findSmallest (x:xs)
  | x < smallestRest = x
  | otherwise = smallestRest
  where smallestRest = findSmallest xs


findLargest :: Ord a => [a] -> a
findLargest [x] = x
findLargest (x:xs)
  | x > largestRest = x
  | otherwise = largestRest
  where largestRest = findLargest xs


calculator :: Fractional a => a -> String -> a -> a
calculator x operation y = 
  case operation of "+" -> x + y
                    "-" -> x - y
                    "*" -> x * y
                    "/" -> x / y


isPalindrome :: String -> Bool
isPalindrome text = text == reverse text


power :: Integer -> Integer -> Integer
power n 0 = 1 
power n k = n * power n (k-1)
                                          

main :: IO ()
main = print (isPalindrome "ba")
