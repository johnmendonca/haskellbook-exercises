module Starman where

check :: String -> String -> Char -> (Bool, String)
check word display c = (elemOf, newDisplay)
  where
    elemOf = c `elem` word
    newDisplay = [if x == c then c else y | (x,y) <- zip word display]

turn :: String -> String -> Int -> IO ()
turn word display n
  | n == 0          = putStrLn "Game Over."
  | word == display = putStrLn "You win."
  | otherwise       = mkguess word display n

mkguess :: String -> String -> Int -> IO ()
mkguess word display n = do
  putStrLn (display ++ "  " ++ replicate n '*')
  putStr "  Enter your guess: "
  q <- getLine
  let (correct, display') = check word display (head q)
  let n' = if correct then n else n-1
  turn word display' n'

starman :: String -> Int -> IO ()
starman word = turn word ['-' | x <- word]

