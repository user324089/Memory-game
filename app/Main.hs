module Main where
import System.Console.ANSI
import Control.Concurrent
import System.IO
import System.Random
import Data.Array

micro_per_second :: Int
micro_per_second = 1000000

game_over :: Int -> IO ()
game_over score = do
    putStrLn $ "You lost with score " ++ show score

clear_previous_lines :: Int -> IO ()
clear_previous_lines 0 = return ()
clear_previous_lines num_lines = do
    cursorUp 1
    clearLine
    clear_previous_lines (num_lines - 1)

show_and_wait :: [String] -> Int -> IO ()
show_and_wait text wait_time = do
    putStr $ unlines text
    Control.Concurrent.threadDelay wait_time
    clear_previous_lines (length text)

main_with_score :: Int -> IO [Char] -> IO ()
main_with_score score random_characters = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    putStrLn $ "Current score: " ++ show score
    wanted_sequence <- fmap (take 10) random_characters
    show_and_wait ["The sequence to remember:", wanted_sequence] (5 * micro_per_second)
    putStrLn "Enter the remembered sequence:"
    clearLine
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    xd <- getLine
    clear_previous_lines 3
    if wanted_sequence == xd
        then do
            show_and_wait ["Correct! Next sequence incoming..."] micro_per_second
            main_with_score (score+1) (fmap (drop 10) random_characters)
        else
            game_over score

data Character_type = Small_letter | Big_letter | Digit | Special_symbol

get_characters_of_type :: Character_type -> [Char]
get_characters_of_type Small_letter = "abcdefghijklmnopqrstuvwxyz"
get_characters_of_type Big_letter = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
get_characters_of_type Digit = "0123456789"
get_characters_of_type Special_symbol = "[]{};:'?!@#$%^&*()_-+=|<>.,"

get_character_list :: [Character_type] -> [Char]
get_character_list types = concat $ map get_characters_of_type types

make_array_from_list :: [a] -> Array Int a
make_array_from_list lst = listArray (0, length lst - 1) lst

random_chars_sequence :: Array Int Char -> IO [Char]
random_chars_sequence possible_chars = do
    std_gen <- getStdGen
    return (map ((!) possible_chars) (randomRs (0, length possible_chars - 1) std_gen))

main :: IO ()
main = main_with_score 0 (random_chars_sequence (make_array_from_list (get_character_list [Digit])))
