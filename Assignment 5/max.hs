-- max.hs
-- Shane Tachick
-- March 31, 2015
-- CS 331
-- Assignment 5

module Max where

-- Looping function to gather integers for comparison
startLoop list = do
  putStr "Enter number (blank line to end): "
  line <- getLine
  let integer = (read line :: Integer)
  if line == "" then
    getMax list
  else do
    startLoop (list++[integer])

-- Gets the max value and prints it, then jumps to the function loop
getMax list = do
  putStr "Maximum: "
  let max = maximum list
  print max
  loop

-- asks the user if they want to find another max, if they do it jumps 
-- to main, if not it jumps to endProgram
loop = do
  putStr "Compute another maximum? [y/n] "
  line <- getLine
  case line of
    "y" -> main
    "n" -> endProgram
    _ -> badInput
      where
        badInput = do
          putStrLn "Invalid Input"
          loop

endProgram = do
  putStrLn "Bye!"
  
  
main = do
  putStrLn ""
  putStrLn "Enter a list of integers, one each line."
  putStrLn "I will find the maximum value in the list."
  putStrLn ""
  startLoop []