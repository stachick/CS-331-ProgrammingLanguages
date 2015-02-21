#!/usr/bin/env lua
-- pa2_test.lua
-- Glenn G. Chappell
-- 8 Feb 2015
--
-- For CS 331 Spring 2015
-- Test Program for Assignment 2 Functions
-- Used in Assignment 2, Exercise A

pa2 = require "pa2"  -- Import pa2 module


-- *********************************************************************
-- Testing Package
-- *********************************************************************


tester = {}
tester.countTests = 0
tester.countPasses = 0

function tester.test(self, success, testName)
    self.countTests = self.countTests+1
    io.write("    Test: " .. testName .. " - ")
    if success then
        self.countPasses = self.countPasses+1
        io.write("passed")
    else
        io.write("********** FAILED **********")
    end
    io.write("\n")
end

function tester.allPassed(self)
    return self.countPasses == self.countTests
end


-- *********************************************************************
-- Definitions for This Test Program
-- *********************************************************************


-- printTable
-- Given a table, prints it in (roughly) Lua literal notation. If
-- parameter is not a table, prints <not a table>.
function printTable(t)
    -- out
    -- Print parameter, surrounded by double quotes if it is a string.
    local function out(p)
        if type(p) == "string" then io.write('"') end
        io.write(p)
        if type(p) == "string" then io.write('"') end
    end

    if type(t) ~= "table" then
        io.write("<not a table>")
    end

    io.write("{ ")
    local first = true  -- First iteration of loop?
    for k, v in pairs(t) do
        if first then
            first = false
        else
            io.write(", ")
        end
        io.write("[")
        out(k)
        io.write("]=")
        out(v)
    end
    io.write(" }")
end


-- printArray
-- Given a table, prints it in (roughly) Lua literal notation for an
-- array. If parameter is not a table, prints <not a table>.
function printArray(t)
    -- out
    -- Print parameter, surrounded by double quotes if it is a string.
    local function out(p)
        if type(p) == "string" then io.write('"') end
        io.write(p)
        if type(p) == "string" then io.write('"') end
    end

    if type(t) ~= "table" then
        io.write("<not a table>")
    end

    io.write("{ ")
    local first = true  -- First iteration of loop?
    for k, v in ipairs(t) do
        if first then
            first = false
        else
            io.write(", ")
        end
        out(v)
    end
    io.write(" }")
end


-- tableEq
-- Compare equality of two tables.
-- Uses "==" on table values. Returns false if either of t1 or t2 is not
-- a table.
function tableEq(t1, t2)
    -- Both params are tables?
    local type1, type2 = type(t1), type(t2)
    if type1 ~= "table" or type2 ~= "table" then
        return false
    end

    -- Get number of keys in t1 & check values in t1, t2 are equal
    local t1numkeys = 0
    for k, v in pairs(t1) do
        t1numkeys = t1numkeys + 1
        if t2[k] ~= v then
            return false
        end
    end

    -- Check number of keys in t1, t2 same
    local t2numkeys = 0
    for k, v in pairs(t2) do
        t2numkeys = t2numkeys + 1
    end
    return t1numkeys == t2numkeys
end


-- getCoroutineValues
-- Given coroutine f, returns array of all values yielded by f when
-- passed param as its parameter, in the order the values are yielded.
function getCoroutineValues(f, param)
    assert(type(f)=="function",
           "getCoroutineValues: f is not a function")
    local covals = {}  -- Array of values yielded by coroutine f
    local co = coroutine.create(f)
    local ok, value = coroutine.resume(co, param)
    while (coroutine.status(co) ~= "dead") do
        table.insert(covals, value)
        ok, value = coroutine.resume(co)
    end
    assert(ok, "Error in coroutine")
    return covals
end


-- *********************************************************************
-- Test Suite Functions
-- *********************************************************************


function test_concatLimit(t)
    local function test(t, ins, lim, expect, msg)
        local outs = pa2.concatLimit(ins, lim)
        local success = outs == expect
        t:test(success, msg)
        if not success then
            io.write("Expect: "..expect.."\n")
            io.write("Actual: "..outs.."\n")
            io.write("\n")
        end
    end

    io.write("Test Suite: concatLimit\n")

    local ins, expect

    ins = "a"
    expect = "aa"
    test(t, ins, 2, expect, "string of length 1, #1")
    expect = "aaaaaaaa"
    test(t, ins, 8, expect, "string of length 1, #2")
    expect = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    test(t, ins, 40, expect, "string of length 1, #3")

    ins="abcdefghijklmnop"
    expect=""
    test(t, ins, 7, expect, "string of length 16, #1")
    test(t, ins, 15, expect, "string of length 16, #2")
    expect = ins
    test(t, ins, 16, expect, "string of length 16, #3")
    expect=ins..ins..ins..ins
    test(t, ins, 70, expect, "string of length 16, #4")
end


function test_mapTable(t)
    local function test(t, f, inv, expect, msg)
        local outv = pa2.mapTable(f, inv)
        local success = tableEq(outv, expect)
        t:test(success, msg)
        if not success then
            io.write("Expect: ")
            printTable(expect)
            io.write("\n")
            io.write("Actual: ")
            printTable(outv)
            io.write("\n")
            io.write("\n")
        end
    end

    io.write("Test Suite: mapTable\n")

    local f, inv, expecg

    f = function(n) return n*n end
    inv = {}
    expect = {}
    test(t, f, inv, expect, "square, empty table")

    inv = {-101}
    expect = {10201}
    test(t, f, inv, expect, "square, singleton array")

    inv = {2,6,17,-4,0,-27,15,50,-1}
    expect = {4,36,289,16,0,729,225,2500,1}
    test(t, f, inv, expect, "square, array of numbers")

    inv = {-5,["x"]=12,[3]=7}
    expect = {25,["x"]=144,[3]=49}
    test(t, f, inv, expect, "square, table of numbers")

    f = function(s) return s:len() end
    inv = {"abc"}
    expect = {3}
    test(t, f, inv, expect,
           "length, singleton array")
    inv = {"12345",["dog"]="Hello there!",[5]="abcdefghijklmnop"}
    expect = {5,["dog"]=12,[5]=16}
    test(t, f, inv, expect, "length, table of strings")

end


function test_collatzSeq(t)
    local function test(t, inv, expect, msg)
        local outv = getCoroutineValues(pa2.collatzSeq, inv)
        local success = tableEq(outv, expect)
        t:test(success, msg)
        if not success then
            io.write("Expect (yielded values): ")
            printArray(expect)
            io.write("\n")
            io.write("Actual (yielded values): ")
            printArray(outv)
            io.write("\n")
            io.write("\n")
        end
    end

    io.write("Test Suite: collatzSeq\n")

    local inv, expect

    inv = 1
    expect = {1}
    test(t, inv, expect, "sequence starting at "..inv)

    inv = 2
    expect = {2,1}
    test(t, inv, expect, "sequence starting at "..inv)

    inv = 3
    expect = {3,10,5,16,8,4,2,1}
    test(t, inv, expect, "sequence starting at "..inv)

    inv = 9
    expect = {9,28,14,7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1}
    test(t, inv, expect, "sequence starting at "..inv)

    inv = 27
    expect = {27,82,41,124,62,31,94,47,142,71,214,107,322,161,484,242,
        121,364,182,91,274,137,412,206,103,310,155,466,233,700,350,175,
        526,263,790,395,1186,593,1780,890,445,1336,668,334,167,502,251,
        754,377,1132,566,283,850,425,1276,638,319,958,479,1438,719,2158,
        1079,3238,1619,4858,2429,7288,3644,1822,911,2734,1367,4102,2051,
        6154,3077,9232,4616,2308,1154,577,1732,866,433,1300,650,325,976,
        488,244,122,61,184,92,46,23,70,35,106,53,160,80,40,20,10,5,16,8,
        4,2,1}
    test(t, inv, expect, "sequence starting at "..inv)
end


function test_pa2(t)
    io.write("TEST SUITES FOR CS 331 ASSIGNMENT 2\n")
    test_concatLimit(t)
    test_mapTable(t)
    test_collatzSeq(t)
end


-- *********************************************************************
-- Main Program
-- *********************************************************************


test_pa2(tester)
io.write("\n")
if tester:allPassed() then
    io.write("All tests successful\n")
else
    io.write("Tests ********** UNSUCCESSFUL **********\n")
end
io.write("\n")

