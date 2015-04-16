-- colt_repl.hs
-- Glenn G. Chappell
-- Started 6 Apr 2015
-- Updated 7 Apr 2015
--
-- For CS 331 Spring 2015
-- REPL for Colt Programming Language
-- (Colt = Cs331's Other Lisp Thing)

module Main where

import ColtLang   -- For Colt language parsing & evaluation functions
import System.IO  -- For hFlush


-- main
-- Do Colt REPL
main = repl


-- myGetLine
-- Quick & dirty line input with backspace.
myGetLine :: IO String
myGetLine = f "" where
    f revline = do
        rawc <- getChar
        let c = (if rawc == '\DEL' then '\b' else rawc)
        if c == '\n' then return (reverse revline)
        else do
            -- Erase backspace & erased char, if any
            putStr $ if c /= '\b' then ""
                     else if revline == "" then "\b\b  \b\b"
                     else "\b\b\b   \b\b\b"
            hFlush stdout  -- Make sure output done before next input
            -- Add the new char to the input
            let revline' = addChar c revline
            -- And do more input
            f revline'
    addChar '\b' [] = []
    addChar '\b' (x:xs) = xs
    addChar c s = c:s


-- readValue
-- Given a prompt, reads a complete Colt expression, asking for
-- additional input if necessary, with a double prompt. Returns
-- parsed version as IO-wrapped Value. Parses using doParse.
readValue :: String -> IO Value
readValue prompt = do
    putStr prompt
    putStr " "
    result <- readValue' prompt ""
    return result
    where
        readValue' :: String -> String -> IO Value
        readValue' prompt soFar = do
            hFlush stdout  -- Make sure prompt comes before input
            newInput <- myGetLine
            let input = soFar ++ " " ++ newInput
            let parsedInput = ColtLang.doParse input
            if isUnfinished parsedInput then do
                putStr prompt
                putStr prompt  -- Double prompt
                putStr " "
                result <- readValue' prompt input
                return result
             else return $ toValue parsedInput
        isUnfinished :: (Show e) => Either e Value -> Bool
        isUnfinished (Left e) = isUnfinished' (show e)
        isUnfinished _ = False
        isUnfinished' :: String -> Bool
        isUnfinished' "" = False
        isUnfinished' ('\n':s) = startsWith s "unexpected end of input"
          -- Above is a kludge; how to check the kind of error correctly?
        isUnfinished' (c:cs) = isUnfinished' cs
        startsWith xs ys = take (length ys) xs == ys
        toValue :: (Show e) => Either e Value -> Value
        toValue (Left errMsg) = Err ("parse\n" ++ show errMsg)
        toValue (Right val) = val


-- ploop
-- Print loop: REPL without "E". Read Colt expression, print it, repeat.
ploop :: IO ()
ploop = do
    val <- readValue ":"
    putStrLn $ show val
    ploop


-- repl
-- Does REPL: read Colt expression, evaluate it, print result, repeat.
repl :: IO ()
repl = doRepl predefs where
    doRepl defs = do
        inval <- readValue ">"
        let (newdefs, outval) = ColtLang.evalWithDef defs inval
        if ColtLang.isNoValue outval then return ()
        else putStrLn $ show outval
        doRepl newdefs

