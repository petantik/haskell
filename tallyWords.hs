module Main where
import System.Environment
import System.IO.Error (isDoesNotExistError)
import Control.Monad (guard)
import Control.Exception (tryJust)
import Data.List
import Data.Tuple (swap)

uniqueWords :: String -> [String]
uniqueWords = nub . words

uniqueWordsSorted :: String -> [String]
uniqueWordsSorted =  sort . uniqueWords

countTokens :: [String] -> [String] -> [Int]
countTokens (t:ts) tokenStream = length matchedTokens : countTokens ts tokenStream
                where matchedTokens =  filter (== t) tokenStream
countTokens _ _ = []

wordTally :: [String] -> [[(String, Int)]]
wordTally (x:xs) = zip tokens tokenCount : wordTally xs
                where tokens = uniqueWordsSorted x
                      tokenStream = words x
                      tokenCount = countTokens tokens tokenStream
wordTally _ = []

sortByCount :: [[(String, Int)]] -> [[(Int, String)]]
sortByCount (x:xs) = sort (flipArgs x): sortByCount xs
                where flipArgs (y:ys) = swap y : flipArgs ys
                      flipArgs _ = []
sortByCount _ = []

main :: IO ()
main = do
    files <- tryJust (guard . isDoesNotExistError) (mapM readFile =<< getArgs)
    case files of
        Right x ->  do 
                print . wordTally $ x
                putStrLn "Sorted Count"
                print . sortByCount . wordTally $ x
        _ -> print "Error all file do not exist"
