module Print3Broken where

printSecond :: String -> IO ()
printSecond g = do
  putStrLn g

main :: IO ()
main = do
  putStrLn greeting
  printSecond greeting
  where greeting = "Yarrr"
