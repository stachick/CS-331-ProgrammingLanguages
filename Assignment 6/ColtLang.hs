-- ColtLang.hs
-- Shane Tachick	
-- CS 331 Programming Languages
-- Glenn G. Chappell
-- Assignment Five

-- Parsing and Evaluation for Colt Programming Language
-- (Colt = CS331's Other Lisp Thing)

module ColtLang where

import Text.ParserCombinators.Parsec

-- type definitions

-- type value
-- holds a lisp value
data Value =
      NoValue
    | Err String
    | Pair Value Value
    | Null
    | Symbol String
    | Number Integer
    | Str String
    | Boolean Bool
    | Func (DefList -> [Value] -> (DefList, Value))

-- showVal
-- show function for valuev
showVal :: Value -> String
showVal NoValue = ""
showVal (Err msg) = "*** ERROR: " ++ msg
showVal (Pair x y) = "(" ++ showVal x ++ showValEndList y where
    showValEndList (Pair x y) = " " ++ showVal x ++ showValEndList y
    showValEndList Null = ")"
    showValEndList z = " . " ++ showVal z ++ ")"
showVal Null = "()"
showVal (Symbol name) = name
showVal (Number n) = show n
showVal (Str s) = "\"" ++ s ++ "\""
showVal (Boolean b) = if b then "#t" else "#f"
showVal (Func _) = "<procedure>"

instance Show Value where show = showVal

-- quoteIt
-- utility function for making quoted values: x -> (quote x)
quoteIt :: Value -> Value
quoteIt x = Pair (Symbol "quote") (Pair x Null)

-- isNoValue
-- ttility function for checking for NoValue
isNoValue :: Value -> Bool
isNoValue NoValue = True
isNoValue _ = False

-- type Deflist
-- holds a list of symbol definitions
-- a DefList is a Haskell list
-- each item is a pair: (String, Value)
-- the Value is the value of the symbol whose name is given by the String
-- symbols with unlisted names are taken to be undefined
-- earlier definitions override later ones
type DefList = [(String, Value)]

-- parsing

-- listToVal
-- convert a Haskell list to a Value
listToVal :: [Value] -> Value
listToVal [] = Null
listToVal (x:xs) = Pair x (listToVal xs)

-- parsing functions

-- doParse
-- given a String, attempt to parse it as a Colt expression
-- return value is return of parser combinator:
-- (Left String) for error message
-- (Right Value) for correctly parsed value
doParse :: String -> Either ParseError Value
doParse input = parse parseExpr "Colt" input

-- special
-- parse a special character that is legal at the start of a symbol
special :: Parser Char
special = oneOf "!$%&|*+-/:<=>?@^_~"

-- skipSpace
-- Skip one or more whitespace characters.
skipSpace :: Parser ()
skipSpace = skipMany1 space

-- parseSymbolNum
-- parse an atom containing only letters, digits, special characters,
-- and '#", not beginnning with '#'
-- determine whether it is a symbol or a number,
-- and return the appropriate Value
parseSymbolNum :: Parser Value
parseSymbolNum = do
   first <- letter <|> digit <|> special
   rest <- many (letter <|> digit <|> special <|> char '#')
   let str = (first:rest)
   if isNumber str then do
       let num = readNumber str
       return (Number num)
   else
       return (Symbol str)
       where
       isNumber ('+':c:cs) = all isDigit (c:cs)
       isNumber ('-':c:cs) = all isDigit (c:cs)
       isNumber (c:cs) = all isDigit (c:cs)
       isNumber _ = False
       isDigit c = c >= '0' && c <= '9'
       readNumber ('+':c:cs) = read (c:cs)  -- No '+' in Haskell Integer
       readNumber str = read str

-- parseStr
-- parse a double-quoted string, e.g., "abc" - no backslash escapes
parseStr :: Parser Value
parseStr = do
    char '"'
    s <- many (noneOf "\"")
    char '"'
    return (Str s)

-- parsePound
-- parse a literal beginning with a pound sign, e.g., #t
-- only handles boolean values
parsePound :: Parser Value
parsePound = do
    char '#'
    c <- oneOf "tf"
    return $ Boolean (c == 't')

-- parseQuoted
-- parse quoted value, like 'x or '(a b c)
parseQuoted :: Parser Value
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ quoteIt x

-- parseListoid
-- parse a list or list plus dot-value at end
-- e.g., (val val val) or (val val . val)
parseListoid :: Parser Value
parseListoid = do
    char '('
    myVal <- parseListoid'
    skipMany space
    char ')'
    return myVal
    where
    parseListoid' :: Parser Value
    parseListoid' = do
        skipMany space
        first <- (try parseExpr <|> return NoValue)
        if isNoValue first then return Null
        else do
            tail <- (try getDotExpr <|> return NoValue)
            if isNoValue tail then do
                rest <- parseListoid'
                return $ Pair first rest
            else return $ Pair first tail
    getDotExpr :: Parser Value
    getDotExpr = do
        skipMany space
        char '.'
        skipMany space
        val <- parseExpr
        return val

-- parseExpr
-- parse a Lisp expression; start symbol in our grammar
parseExpr :: Parser Value
parseExpr = do
    skipMany space
    parseSymbolNum <|>
      parseStr <|>
      parsePound <|>
      parseQuoted <|>
      parseListoid

-- evaluation

-- err
-- given function name and message
-- returns Value representing evaluation error
err :: String -> String -> Value
err fn msg = Err ("evaluation\n" ++ fn ++ ": " ++ msg)

-- valToList
-- convert a Value (must be Pair or Null) to a Haskell list
valToList :: Value -> [Value]
valToList Null = []
valToList (Pair x y) = x:valToList y
valToList _ = [err "valToList (internal func)"
                   "Value is not Pair or Null; cannot convert to list"]

-- evalWithDef
-- given Value to evaluate, and context (existing defines)
-- returns pair: result of evaluation and new context
evalWithDef :: DefList -> Value -> (DefList, Value)
evalWithDef d (Pair h pl) = callFunc (evalExpr d h) d (valToList pl) where
    callFunc (Func f) d ps = f d ps
    callFunc (Err e) d _ = (d, Err e)
    callFunc _ d _ = (d, err "eval" "No procedure call")
evalWithDef d (Symbol name) = (d, symbolVal $ lookup name d) where
    symbolVal (Just v) = v
    symbolVal Nothing = err "eval" ("Symbol `" ++ name ++ "' not defined")
evalWithDef d Null = (d, err "eval" "No procedure call")
evalWithDef d v = (d, v)
-- Above line for: NoValue, Err, Number, Str, Boolean, Func

-- evalExpr
-- given Value to evaluate, and context (existing defines)
-- returns result of evaluation
evalExpr :: DefList -> Value -> Value
evalExpr d v = v' where
    (_, v') = evalWithDef d v

-- normFunc
-- make a Func value out of a "normal" function
normFunc :: ([Value] -> Value) -> Value
normFunc nf = Func f where
    f d vs = (d, nf $ map (evalExpr d) vs)

-- implementation of special primitive procedures
-- see predefs (below) for symbol-primitive bindings

sfuncDefine :: DefList -> [Value] -> (DefList, Value)
sfuncDefine d [Symbol s, v] = (d', NoValue) where
    d' = (s, v'):d
    v' = evalExpr d' v
sfuncDefine d _ = (d, err "define" "Must pass symbol, value")

sfuncEval :: DefList -> [Value] -> (DefList, Value)
sfuncEval d [v] = evalWithDef d' v' where
    (d', v') = evalWithDef d v
sfuncEval d _ = (d, err "eval" "Must pass single value")

sfuncQuote :: DefList -> [Value] -> (DefList, Value)
sfuncQuote d [v] = (d, v)
sfuncQuote d _ = (d, err "quote" "Must pass single value")

sfuncLambda :: DefList -> [Value] -> (DefList, Value)
sfuncLambda d [Symbol name, v] = (d, Func f) where
    f dd ps = (dd, evalExpr d' v) where
        paramdef = (name, listToVal $ map (evalExpr dd) ps)
        d' = paramdef:d
sfuncLambda d [sl, v]
  | isSymbolList sl  = (d, Func f) where
    f dd ps
      | length ps == length ss  = (dd, evalExpr d' v) where
        ss = map symbolName $ valToList sl
        symbolName (Symbol name) = name
        paramdefs = zip ss (map (evalExpr dd) ps)
        d' = paramdefs ++ d
    f dd _ = (dd, err "(lambda func)" "Must pass parameter(s), expression")
    isSymbolList Null = True
    isSymbolList (Pair (Symbol _) rest) = isSymbolList rest
    isSymbolList _ = False
sfuncLambda d _ = (d, err "lambda" "Must pass parameter(s), expression")

-- implementation of "normal" primitive procedures:
-- all parameters eval'd, no global bindings created or modified
-- see predefs (below) for symbol-primitive bindings

nfuncCar :: [Value] -> Value
nfuncCar [Pair x _] = x
nfuncCar _ = err "car" "Must pass pair"

nfuncCdr :: [Value] -> Value
nfuncCdr [Pair _ y] = y
nfuncCdr _ = err "cdr" "Must pass pair"

nfuncEqual :: [Value] -> Value
nfuncEqual [Number x, Number y] = Boolean (x == y)
nfuncEqual _ = err "=" "Function requires two numbers"

nfuncLT :: [Value] -> Value
nfuncLT [Number x, Number y] = Boolean (x < y)
nfuncLT _ = err "<" "Function requires two numbers"

nfuncPlus :: [Value] -> Value
nfuncPlus ns = nfuncPlus' 0 ns where
    nfuncPlus' sofar [] = Number sofar
    nfuncPlus' sofar (Number n:ns) = nfuncPlus' (sofar+n) ns
    nfuncPlus' _ _ = err "+" "All parameters must be numbers"

nfuncMinus :: [Value] -> Value
nfuncMinus [] = err "-" "At least one (number) parameter required"
nfuncMinus [Number n] = Number (-n)
nfuncMinus (Number n:ns) = nfuncMinus' n ns where
    nfuncMinus' sofar [] = Number sofar
    nfuncMinus' sofar (Number n:ns) = nfuncMinus' (sofar-n) ns
    nfuncMinus' _ _ = err "-" "All parameters must be numbers"
nfuncMinus _ = err "-" "All parameters must be numbers"

nfuncIf :: [Value] -> Value
nfuncIf [Boolean True, x, _] = x
nfuncIf [Boolean False, _, y] = y
nfuncIf _ = err "if" "Must pass boolean, two other values"

nfuncCons :: [Value] -> Value
nfuncCons [a, b] = Pair a b
nfuncCons _ = err "cons" "Must pass two parameters"

nfuncList :: [Value] -> Value
nfuncList [] = Null
nfuncList (v:vs) = Pair v (nfuncList vs)

nfuncError :: [Value] -> Value
nfuncError [] = Err ("program-generated\n" ++ "Execution error")
nfuncError [Str msg] = Err ("program-generated\n" ++ msg)
nfuncError _ = err "error" "Must pass no parameters or one string"

nfuncTimes :: [Value] -> Value
nfuncTimes ns = nfuncTimes' 1 ns where
    nfuncTimes' sofar [] = Number sofar
    nfuncTimes' sofar (Number n:ns) = nfuncTimes' (sofar*n) ns
    nfuncTimes' _ _ = err "*" "All parameters must be numbers"

nfuncLessThanEqual :: [Value] -> Value
nfuncLessThanEqual [Number x, Number y] = Boolean (x <= y)
nfuncLessThanEqual _ = err "<=" "Function requires two numbers"

nfuncGreaterThan :: [Value] -> Value
nfuncGreaterThan [Number x, Number y] = Boolean (x > y)
nfuncGreaterThan _ = err ">" "Function requires two numbers"

nfuncGreaterThanEqual :: [Value] -> Value
nfuncGreaterThanEqual [Number x, Number y] = Boolean (x >= y)
nfuncGreaterThanEqual _ = err ">=" "Function requires two numbers"

nfuncQuotient :: [Value] -> Value
nfuncQuotient [Number x, Number y] = Number (quot x y)
nfuncQuotient _ = err "quotient" "Function requires two numbers"

nfuncRemainder :: [Value] -> Value
nfuncRemainder [Number x, Number y] = Number (rem x y)
nfuncRemainder _ = err "remainder" "Function requires two numbers"

nfuncNull :: [Value] -> Value
nfuncNull [] = Boolean True
nfuncNull [_] = Boolean False
nfuncNull _ = err "null?" "How did you get an error? It is a true/false question, there are no errors!"

nfuncPair :: [Value] -> Value
nfuncPair [Pair _ _] = Boolean True
nfuncPair [_] = Boolean False
nfuncPair _ = err "pair?" "How did you get an error? It is a true/false question, there are no errors!"

nfuncNumber :: [Value] -> Value
nfuncNumber [Number _] = Boolean True
nfuncNumber [_] = Boolean False
nfuncNumber _ = err "number?" "How did you get an error? It is a true/false question, there are no errors!"

nfuncString :: [Value] -> Value
nfuncString [Str _] = Boolean True
nfuncString [_] = Boolean False
nfuncString _ = err "string?" "How did you get an error? It is a true/false question, there are no errors!"

nfuncBoolean :: [Value] -> Value
nfuncBoolean [Boolean _] = Boolean True
nfuncBoolean [_] = Boolean False
nfuncBoolean _ = err "boolean?" "How did you get an error? It is a true/false question, there are no errors!"

nfuncStringEquality :: [Value] -> Value
nfuncStringEquality [Str a, Str b]
	| (compare a b == EQ) = Boolean True
	| otherwise = Boolean False
nfuncStringEquality [_, _] = err "string=?" "Both arguments must be strings"
nfuncStringEquality _ = err "string=?" "Must pass two strings"

nfuncStringLength :: [Value] -> Value
nfuncStringLength [Str a] = Number (toInteger $ length a)
nfuncStringLength _ = err "string-length" "Must pass one string"

nfuncAnd :: [Value] -> Value
nfuncAnd ns = nfuncAnd' True ns where
    nfuncAnd' sofar [] = Boolean sofar
    nfuncAnd' sofar (Boolean n:ns) = nfuncAnd' (sofar&&n) ns
    nfuncAnd' _ _ = err "and" "All parameters must be boolean"

nfuncOr :: [Value] -> Value
nfuncOr ns = nfuncOr' False ns where
    nfuncOr' sofar [] = Boolean sofar
    nfuncOr' sofar (Boolean n:ns) = nfuncOr' (sofar||n) ns
    nfuncOr' _ _ = err "or" "All parameters must be boolean"

nfuncNot :: [Value] -> Value
nfuncNot [Boolean True] = Boolean False
nfuncNot [Boolean False] = Boolean True
nfuncNot _ = err "not" "Must pass a boolean"

nfuncMap :: [Value] -> Value
nfuncMap _ = err "map" "shit broke"

nfuncFilter :: [Value] -> Value
nfuncFilter _ = err "filter" "shit broke"

nfuncApply :: [Value] -> Value
nfuncApply _ = err "apply" "shit broke"

nfuncToString :: [Value] -> Value
nfuncToString _ = err "to-string" "shit broke"

nfuncStringConcat :: [Value] -> Value
nfuncStringConcat _ = err "string-concat" "shit broke"

nfuncStringSub :: [Value] -> Value
nfuncStringSub _ = err "string-sub" "shit broke"

nfuncTake :: [Value] -> Value
nfuncTake _ = err "take" "shit broke"

-- predefs
-- holds name-value pairs for pre-defined symbols: symbol name, value
predefs :: DefList
predefs = [
    ("define", Func sfuncDefine),
    ("eval", Func sfuncEval),
    ("quote", Func sfuncQuote),
    ("lambda", Func sfuncLambda),
    ("car", normFunc nfuncCar),
    ("cdr", normFunc nfuncCdr),
    ("=", normFunc nfuncEqual),
    ("<", normFunc nfuncLT),
    ("+", normFunc nfuncPlus),
    ("-", normFunc nfuncMinus),
    ("if", normFunc nfuncIf),
    ("cons", normFunc nfuncCons),
    ("list", normFunc nfuncList),
    ("error", normFunc nfuncError),
    ("*", normFunc nfuncTimes),
    ("<=", normFunc nfuncLessThanEqual),
    (">", normFunc nfuncGreaterThan),
    (">=", normFunc nfuncGreaterThanEqual),
    ("quotient", normFunc nfuncQuotient),
    ("remainder", normFunc nfuncRemainder),
    ("null?", normFunc nfuncNull),
    ("pair?", normFunc nfuncPair),
    ("number?", normFunc nfuncNumber),
    ("string?", normFunc nfuncString),
    ("boolean?", normFunc nfuncBoolean),
    ("string=?", normFunc nfuncStringEquality),
    ("string-length", normFunc nfuncStringLength),
    ("and", normFunc nfuncAnd),
    ("or", normFunc nfuncOr),
    ("not", normFunc nfuncNot),
    ("map", normFunc nfuncMap),
    ("filter", normFunc nfuncFilter),
    ("apply", normFunc nfuncApply),
    ("to-string", normFunc nfuncToString),
    ("string-concat", normFunc nfuncStringConcat),
    ("string-sub", normFunc nfuncStringSub),
    ("take", normFunc nfuncTake)]

