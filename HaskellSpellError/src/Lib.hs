module Lib
    (
        spellCheck,
    ) where

import System.IO
import qualified Data.Text as T
import Data.List as L
import Control.Monad as M

spellCheck :: IO () --Only outputs once so all of this will output at the end
spellCheck = do let debug = False

                df <- stringInput "Enter name of dictionary file"
                putStrLn ("Reading: " ++ df)
                dfContent <- readInFile df
                splitDict <- stringSplit dfContent "\n"

                let stringDict = textToString2 splitDict
                M.when debug $ print stringDict--Alternative to if else using Control.Monad

                --Spell Checked File Preperation
                cf <- stringInput "Enter name of the file that will be spell checked"
                putStrLn ("Reading: " ++ cf)
                cfContent <- readInFile cf
                putStrLn ("Contents of file \"" ++ cfContent ++ "\"")
                splitCheck <- stringSplit cfContent " "


                let stringCheck = textToString2 splitCheck
                M.when debug $ print stringCheck

                let invalidWords = checkDict2 stringCheck stringDict
                M.when debug $ print invalidWords

                cleanedWords <- wordCleaner invalidWords stringDict

                print cleanedWords

                let cleanedSentence = wordReplacer cleanedWords stringCheck

                print cleanedSentence

                putStrLn (L.unwords cleanedSentence)



stringInput :: [Char] -> IO [Char]
stringInput x = do
    putStrLn x
    getLine

readInFile :: String -> IO String
readInFile = readFile
--readInFile a = readFile a

stringSplit :: [Char] -> [Char] -> IO [T.Text]
stringSplit stringIn splitBy = do return (T.splitOn (T.pack splitBy) (T.pack stringIn))

--Tail Recursion
textToString2 :: [T.Text] -> [String]
textToString2 xs = do helper xs where
    helper [] = []
    helper (x:xs) = if T.length x > 0 then T.unpack x : helper xs else helper xs

--Tail Recursion
checkDict2 :: [[Char]] -> [[Char]] -> [([Char], Int)]
checkDict2 cs ds = do helper cs ds 0 where
    helper cs [] _ = []
    helper [] ds _ = []
    helper (c:cs) ds x = do if checkWord c ds then helper cs ds (x+1) else (c,x) : helper cs ds (x+1)

checkWord :: (Eq a1) => a1 -> [a1] -> Bool
checkWord w ds = w `elem` ds

wordCleaner :: [([Char], Int)] -> [[Char]] -> IO [([Char], Int)]
wordCleaner is [] = return []
wordCleaner [] ds = return []
wordCleaner (i:is) ds = do if checkWord (fst i) ds
                            then do
                                xs <- wordCleaner is ds
                                return (i:xs)
                            else do
                                x <- retype1 (fst i) ds
                                xs <- wordCleaner is ds
                                return ((x ,snd i):xs)


retype1 :: [Char] -> [[Char]]-> IO [Char]
retype1 i ds = do
                x <- stringInput ( "Please retype the word: " ++ i)
                if checkWord x ds
                  then do
                      return x
                  else do
                      retype1 x ds

wordReplacer :: [([Char], Int)] -> [[Char]] -> [[Char]]
wordReplacer ws [] = []
wordReplacer [] ss = ss
wordReplacer (w:ws) ss = do
    let index = snd w
    -- let a = take index ss
    -- let b = reverse (take (length ss -index -1) (reverse ss))
    -- let xs = a ++ [fst w] ++ b
    let xs = take index ss ++ [fst w] ++ reverse (take (length ss -index -1) (reverse ss))
    wordReplacer ws xs


--Code Graveyard 

-- retype :: [Char] -> [[Char]]-> IO [Char]
-- retype i ds = stringInput ( "Please retype the word: " ++ i)

-- cleanWord :: [Char] -> [[Char]] -> IO [Char]
-- cleanWord i ds = if checkWord i ds 
--                     then return i
--                     else cleanWord (retype i) ds


-- Retype take in [("N", 2)] ds -> [("N", 2)]
-- Ask to retype word
-- Check Retyped word (can be done by recalling this function in on itself me thinks)


-- Check Spelling of indexed word
-- If incorrect as to retype
-- Check spelling again though recursive call
-- if correct move onto next word


-- wordCleaner :: Eq a1 => [(a1, b)] -> [a1] -> [(a1, b)]
-- wordCleaner is ds = helper is ds where
--     helper is [] = []
--     helper [] ds = []
--     helper (i:is) ds = if checkWord (fst i) ds
--         then i : helper is ds
--         else retype i : helper is ds


-- wordCleaner :: Eq a1 => [(a1, b)] -> [a1] -> IO [(a1, b)]
-- wordCleaner is ds = do helper is ds where
--     helper is [] = return []
--     helper [] ds = return []
--     helper (i:is) ds = do if checkWord (fst i) ds
--                             then (i : helper is ds)
--                             else (retype fst i : helper is ds)


-- --Recursion
-- checkDict1 :: Eq a => [[a]] -> [[a]] -> [[a]]
-- checkDict1 cs [] = []
-- checkDict1 [] ds = []
-- checkDict1 (c:cs) ds = if c `elem` ds then checkDict1 cs ds else c : checkDict1 cs ds


-- --Recursion
-- textToString1 :: [T.Text] -> [String]
-- textToString1 [] = []
-- textToString1 (x:xs) = if T.length x > 0 then T.unpack x : textToString1 xs else textToString1 xs

                --filteredString <- return (filter (not . null) splitString) 


-- retype :: [Char] -> [[Char]] -> [Char]
-- retype i ds = if checkWord i ds then i else retype (stringInput ( "Please retype the word: " ++ i)) ds


-- filterEmpty :: [T.Text] -> IO [T.Text]
-- filterEmpty xs = do return ( filterEmpty xs)


-- stringSplit :: [Char] -> [Char] -> IO [Char]
-- stringSplit stringIn splitBy = do return (stringIn ++ splitBy)


-- someFunc :: IO () --Only outputs once so all of this will output at the end
-- someFunc = do yeet <- mystery2 1 2
--               putStrLn ((show yeet) ++ " is the result produced")


-- mystery2 :: Int -> Int -> IO Int
-- mystery2 v1 v2 = do
--     putStrLn "Enter a number"
--     v3i <- getLine
--     let v3 = read v3i :: Int
--     putStrLn ("Entered " ++ (show(v3)))
--     return ((v1 + v2 + v3)^2)