module Main where
import System.Console.ANSI
import Control.Concurrent
import System.IO
import System.Random
import Data.Array
import Options.Applicative

micro_per_second :: Float
micro_per_second = 1000000

default_sequence_length :: Int
default_sequence_length = 5

data Character_type = Lower_letter | Upper_letter | Digit | Special_symbol

available_character_types :: [Character_type]
available_character_types = [Digit]

default_sequence_exposure_length :: Float
default_sequence_exposure_length = 3.0

default_congratulations_show_length :: Float
default_congratulations_show_length = 1.0

data Run_parameters = Run_parameters {
    p_use_lower_letters :: Bool,
    p_use_upper_letters :: Bool,
    p_use_digits :: Bool,
    p_use_special_symbols :: Bool,
    p_sequence_length :: Int,
    p_sequence_exposure_duration :: Float
}

use_lower_letters_parser :: Parser Bool
use_lower_letters_parser = switch (
    long "lower"
    <> short 'l'
    <> help "Use lower letters in sequences")

use_upper_letters_parser :: Parser Bool
use_upper_letters_parser = switch (
    long "upper"
    <> short 'u'
    <> help "Use upper letters in sequences")

use_digits_parser :: Parser Bool
use_digits_parser = switch (
    long "digits"
    <> short 'd'
    <> help "Use digits in sequences")

use_specials_parser :: Parser Bool
use_specials_parser = switch (
    long "specials"
    <> short 's'
    <> help "Use special characters in sequences")

sequence_length_parser :: Parser Int
sequence_length_parser = option auto (
    long "number"
    <> short 'n'
    <> value default_sequence_length
    <> help "Number of characters in sequences")

exposure_duration_parser :: Parser Float
exposure_duration_parser = option auto (
    long "time"
    <> short 't'
    <> value default_sequence_exposure_length
    <> help "Time duration of sequene exposure")

option_parser :: Parser Run_parameters
option_parser = Run_parameters
    <$>  use_lower_letters_parser
    <*> use_upper_letters_parser
    <*> use_digits_parser
    <*> use_specials_parser
    <*> sequence_length_parser
    <*> exposure_duration_parser

opts :: ParserInfo Run_parameters
opts = info (option_parser <**> helper)
    ( fullDesc
      <> progDesc "Simple game that shows you a sequence that you later have to replicate")

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

main_instance :: Run_parameters -> Int -> IO [Char] -> IO ()
main_instance parameters score random_characters = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    putStrLn $ "Current score: " ++ show score
    let sequence_length = p_sequence_length parameters
    let exposure_duration = p_sequence_exposure_duration parameters
    wanted_sequence <- fmap (take sequence_length) random_characters
    show_and_wait ["The sequence to remember:", wanted_sequence] (
        round (exposure_duration * micro_per_second))
    putStrLn "Enter the remembered sequence:"
    clearLine
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    xd <- getLine
    clear_previous_lines 3
    if wanted_sequence == xd
        then do
            show_and_wait ["Correct! Next sequence incoming..."] (round (micro_per_second * default_congratulations_show_length))
            main_instance parameters (score+1) (fmap (drop sequence_length) random_characters)
        else
            game_over score

get_characters_of_type :: Character_type -> [Char]
get_characters_of_type Lower_letter = "abcdefghijklmnopqrstuvwxyz"
get_characters_of_type Upper_letter = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
get_characters_of_type Digit = "0123456789"
get_characters_of_type Special_symbol = "[]{};:'?!@#$%^&*()_-+=|<>.,"

get_character_list :: [Character_type] -> [Char]
get_character_list types = concat $ map get_characters_of_type types

make_array_from_list :: [a] -> Array Int a
make_array_from_list lst = listArray (0, length lst - 1) lst

take_positives :: [(a, Bool)] -> [a]
take_positives [] = []
take_positives ((x, True):xs) = x:(take_positives xs)
take_positives ((_, False):xs) = take_positives xs

get_types_from_parse_opts :: Run_parameters -> [Character_type]
get_types_from_parse_opts (Run_parameters use_lower_letters use_upper_letters use_digits use_special_symbols _ _) = take_positives [
                                    (Lower_letter, use_lower_letters),
                                    (Upper_letter, use_upper_letters),
                                    (Digit, use_digits),
                                    (Special_symbol, use_special_symbols)]

random_chars_sequence :: Array Int Char -> IO [Char]
random_chars_sequence possible_chars = do
    std_gen <- getStdGen
    return (map ((!) possible_chars) (randomRs (0, length possible_chars - 1) std_gen))

is_list_empty :: [a] -> Bool
is_list_empty [] = True
is_list_empty _ = False

main :: IO ()
main = do
    options <- execParser opts
    let used_types = get_types_from_parse_opts options
    if is_list_empty used_types
        then do
            putStrLn ("Expected at least one of the options: l, u, d, s")
        else do
            let randomised_chars = random_chars_sequence (make_array_from_list (get_character_list used_types))
            main_instance options 0 randomised_chars
