-- pa5_test.hs
-- Glenn G. Chappell
-- 24 Mar 2015
--
-- For CS 331 Spring 2015
-- Test Program for Assignment 5 Functions & Variables
-- Used in Assignment 5, Exercise A

module Main where

import qualified PA5  -- For Assignment 5 Functions & Variables


------------------------------------------------------------------------
-- Testing Package
------------------------------------------------------------------------


-- TestState a
-- Data type for holding results of tests
-- First item in pair is a Maybe giving pass/fail results so far:
--     Just _   means all passed so far
--     Nothing  means at least one failure so far
-- Second item in pair is IO for output of tests
data TestState a = TS (Maybe a, IO a)

-- Accessor functions for parts of a TestState value
tsMaybe (TS (x, y)) = x
tsIO (TS (x, y)) = y

-- Make TestState a monad in the obvious way
instance Monad TestState where
    return a = TS (return a, return a)
    TS (x, y) >>= f = TS (x >>= f1, y >>= f2) where
        f1 = tsMaybe . f
        f2 = tsIO . f

-- testMsg
-- Print a message (e.g., "Test Suite: ...") in TestState monad
testMsg :: String -> TestState ()
testMsg str = TS (Just (), putStrLn str)

-- test
-- Do test in TestState monad
-- Given result of test (Bool: True if passed) & description of test
-- Adds result of test & description + pass/fail output to monadic value
test :: Bool -> String -> TestState ()
test success descrip = TS (theMaybe, putStrLn fullDescrip) where
    theMaybe  | success    = Just ()
              | otherwise  = Nothing
    fullDescrip = "    Test: " ++ descrip ++ " - " ++ passFailStr
    passFailStr  | success    = "passed"
                 | otherwise  = "********** FAILED **********"

-- testEq
-- Like test, but given 2 values, checks whether they are equal
testEq :: Eq a => a -> a -> String -> TestState ()
testEq a b str = test (a == b) str

-- printResults
-- Converts TestState monadic value to IO monadic value
--  with summary of all test results
printResults :: TestState () -> IO ()
printResults z = do
    -- Do IO from tests
    tsIO z
    putStrLn ""
    -- Output summary: all passed or not
    putStrLn $ summaryString z
    where
        summaryString (TS (Just _, _)) = "All tests successful"
        summaryString _ = "Tests ********** UNSUCCESSFUL **********"


------------------------------------------------------------------------
-- Test Suites
------------------------------------------------------------------------


-- test_collatzCounts
-- Test Suite for variable collatzCounts
test_collatzCounts = do
    testMsg "Test Suite: Variable collatzCounts"
    testEq (take 10 PA5.collatzCounts) [0,1,7,2,5,8,16,3,19,6]
        "collatzCounts, example from Assn 5 description"
    test (corrCounts 200 PA5.collatzCounts)
        "collatzCounts, first 200 values"
    testEq (PA5.collatzCounts !! 100000) 89
        "collatzCounts, later value"
    where
        corrCounts len list = corrCounts' len list 1
        corrCounts' 0 _ _ = True
        corrCounts' len [] _ = False
        corrCounts' len (t:ts) n = corrCount n t &&
            corrCounts' (len-1) ts (n+1)
        corrCount n t = t >= 0 && iterCol t n == 1 &&
            (t < 3 || iterCol (t-3) n /= 1)
        iterCol 0 n = n
        iterCol t n = coll $ iterCol (t-1) n
        coll n = if mod n 2 == 1 then 3*n+1 else div n 2


-- test_findList
-- Test Suite for function findList
test_findList = do
    testMsg "Test Suite: Function findList"
    testEq (PA5.findList "cde" "abcdefg") (Just 2)
        "findList, example #1 from Assn 5"
    testEq (PA5.findList "cdX" "abcdefg") Nothing
        "findList, example #2 from Assn 5"
    testEq (PA5.findList [1] [2,1,2,1,2]) (Just 1)
        "findList, example #3 from Assn 5"
    testEq (PA5.findList [] [1,2,3,4,5]) (Just 0)
        "findList, example #4 from Assn 5"
    testEq (PA5.findList [20..25] [3..100]) (Just 17)
        "findList, longer list: found"
    testEq (PA5.findList ([20..25]++[1]) [3..100]) Nothing
        "findList, longer list: not found"
    testEq (PA5.findList ([20..25]++[1]) [3..24]) Nothing
        "findList, longer list: not found at end"
    testEq (PA5.findList ([20..25]++[1]) ([3..30]++[20..25]++[1])) (Just 28)
        "findList, longer list: found 2nd time"


-- test_op_doubleSharp
-- Test Suite for operator ##
test_op_doubleSharp = do
    testMsg "Test Suite: Infix Operator ##"
    testEq ([1,2,3,4,5] PA5.## [1,1,3,3,9,9,9,9,9]) 2
        "op ##, example #1 from Assn 5"
    testEq ([] PA5.## [1,1,3,3,9,9,9,9,9]) 0
        "op ##, example #2 from Assn 5"
    testEq ("abcde" PA5.## "aXcXeX") 3
        "op ##, example #3 from Assn 5"
    testEq ("abcde" PA5.## "XaXcXeX") 0
        "op ##, example #4 from Assn 5"
    testEq (biglist1 PA5.## biglist2) 333
        "op ##, long lists" where
        biglist1 = filter (\n -> mod n 3 /= 0) [1..1000]
        biglist2 = 0:filter (\n -> mod n 3 /= 1) [1..1000]


-- allTests
-- Run all test suites for Assignment 5 functions & variables
allTests = do
    testMsg "TEST SUITES FOR ASSIGNMENT 5 FUNCTIONS & VARIABLES"
    test_collatzCounts
    test_findList
    test_op_doubleSharp


-- main
-- Do all test suites & print results.
main = printResults allTests

