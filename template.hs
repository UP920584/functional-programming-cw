--
-- MATHFUN
-- UP920584

import Data.Function (on)
import Data.List (minimumBy)
import Control.Monad (forM_)

--
-- Types (define City type here)
--
type Location = (Int, Int) 
type City = (String, Location, [Int])

testData :: [City]
testData = [
    ("Amsterdam",    (52,   5),    [1158, 1149, 1140, 1132]),
    ("Athens",       (38,  23),    [3153, 3153, 3154, 3156]),
    ("Berlin",       (53,  13),    [3567, 3562, 3557, 3552]),
    ("Brussels",     (51,   4),    [2096, 2081, 2065, 2050]),
    ("Bucharest",    (44,  26),    [1794, 1803, 1812, 1821]),
    ("London",       (52,   0),    [9426, 9304, 9177, 9046]),
    ("Madrid",       (40,   4),    [6669, 6618, 6559, 6497]),
    ("Paris",        (49,   2),    [11079, 11017, 10958, 10901]),
    ("Rome",         (42,  13),    [4278, 4257, 4234, 4210]),
    ("Sofia",        (43,  23),    [1284, 1281, 1277, 1272]),
    ("Vienna",       (48,  16),    [1945, 1930, 1915, 1901]),
    ("Warsaw",       (52,  21),    [1790, 1783, 1776, 1768])]

testData2 :: [City]
testData2 = [
    ("Amsterdam",    (50,   80),    [1158, 1149, 1140, 1132]),
    ("Athens",       (0,  80),    [3153, 3153, 3154, 3156]),
    ("Berlin",       (50,  0),    [3567, 3562, 3557, 3552]),
    ("Brussels",     (0,   0),    [2096, 2081, 2065, 2050])]

--
--  Your functional code goes here
--

demo1 :: [City] -> [String]
demo1 citiesArray = [n |(n, l, p) <- citiesArray ]

demo2 :: String -> Int -> Int 
demo2 name time = head [p!!time|(n, l, p) <- testData, n == name]

addWhiteSpace :: String -> Int -> String
addWhiteSpace string l
 | length string == l = string
 | otherwise = addWhiteSpace (string ++ " ") l


headings = ("Name", "Location N", "Location E", "Population(Current)", "Population(Last year)")

citiesToString :: [City] -> String
citiesToString citiesArray =  concat [
    addWhiteSpace n 15 ++ addWhiteSpace north 11 ++ addWhiteSpace east 11 ++ addWhiteSpace c 21 ++ addWhiteSpace o 21 ++ "\n" 
    | (n, north, east, c, o) <- lines]
    
    where 
        lines = headings:[(n, show north, show east, show c, show l) | (n, (north, east), c:l:p) <- citiesArray]


updatePopulations :: [Int] -> [City] -> [City]
updatePopulations [] [] = []
updatePopulations (u:us) ((n, l, p):cs) = (n, l, u:(init p)): updatePopulations us cs

insertCity :: City -> [City] -> [City]
insertCity c [] = [c]
insertCity (newName, newLocation, newPopulation) ((n, l, p):cs) 
    | newName < n = (newName, newLocation, newPopulation):(n, l, p):cs
    | otherwise = (n, l, p):insertCity (newName, newLocation, newPopulation) cs

growth :: [Int] -> [Double]
growth [] = []
growth [_] = []
growth (c:cs) = ((fromIntegral c- fromIntegral (head cs) )/ fromIntegral (head cs) )*100: growth cs

showGrowth :: String -> String
showGrowth c = c ++ " " ++ head [concat [show g ++ " "| g <- (growth p)] | (n, l, p) <- testData, c == n]

distance :: Location -> Location -> Float 
distance (x1 , y1) (x2 , y2) = sqrt (fromIntegral ((x1 - x2)^2 + (y1 - y2)^2))

nearestCity :: Location -> Int -> (String, Float)
nearestCity location population = minimumBy (compare `on` snd) [(n, (distance l location)) | (n, l, p) <- testData, head p > population] 


--
--  Demo
--

demo :: Int -> IO ()
demo 1 = print  testData -- output the names of all the cities
demo 2 = print (demo2 "Madrid" 2)-- output the population of "Madrid" 2 years ago
demo 3 = putStrLn (citiesToString testData)
demo 4 = putStrLn (
    citiesToString (updatePopulations [1200,3200,3600,2100,1800,9500,6700,11100,4300,1300,2000,1800] testData)
    )
         -- output the data (as for (iii)) after it has been updated with the
         -- following new population figures (the first is for Amsterdam, etc.)
         -- [1200,3200,3600,2100,1800,9500,6700,11100,4300,1300,2000,1800]
demo 5 = putStrLn (citiesToString (insertCity ("Prague", (50, 14), [1312, 1306, 1299, 1292]) testData))
         -- show the data (as for (iii)) after adding "Prague" (50N, 14E) 
         -- with population figures [1312, 1306, 1299, 1292]
demo 6 = -- output a list of annual growth figures for "London"
         print (showGrowth "London")
demo 7 = -- output the nearest city to location (54N ,6E) with 
         -- a population above 2m people
          print (nearestCity (54, 6) 2000 )
demo 8 = -- output the population map
         mapper


--
-- Screen Utilities (use these to do the population map)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your population map code goes here
--

showCity :: City -> IO ()
showCity (n, (north, east), p)  = writeAt r ("+" ++ n ++ " " ++ show (head p))
    where
        r = (( east `div` 80), ( north `div` 50))

mapper :: IO ()
mapper = do 
    clearScreen
    forM_ testData showCity


--
-- Your user interface (and loading/saving) code goes here
--
 
