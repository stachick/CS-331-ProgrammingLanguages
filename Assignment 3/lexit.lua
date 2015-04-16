--lexit.lua
--Shane Tachick
--CS 331 - programming Languages
--February 18,2015
--Homework 3

local lexerit = {}

lexerit.catnames =
{
	"Identifier",		--Category 1
    "Keyword",			--Category 2
    "Operator",			--Category 3
    "NumericLiteral",	--Category 4
    "StringLiteral",	--Category 5
    "Punctuation",		--Category 6
    "Malformed"			--Category 7
}

local function isDigit(c)
    if c:len() ~= 1 then
        return false
    elseif c >= "0" and c <= "9" then
        return true
    else
        return false
    end
end

local function isLetter(c)
    if c:len() ~= 1 then
        return false
    elseif c >= "a" and c <= "z" then
        return true
	elseif c >= "A" and c <= "Z" then
        return true
    else
        return false
    end
end

local function isSpace(c)
    if c:len() ~= 1 then
        return false
    elseif c == " " or c == "\t" or c == "\f" or c == "\n" or c == "\r" then
        return true
    else
        return false
    end
end

local preferOp = false

function lexerit.preferOp()
    preferOp = true
end

--Lexer Module
function lexerit.lex(code)

	--States
	local DONE =				0
	local START = 				1
	local LETTER =				2
	local DIGIT = 				3
	local PLUS = 				4
	local MINUS = 				5
	local DOT = 				6
	local DIGITDOT =			7
	local EXCLAMATION =			8
	local GREATERTHAN =			9
	local LESSTHAN = 			10
	local EQUAL = 				11
	local SCINOTATION =			12
	local SCINOTATIONSIGN =		13
	local SCINOTATIONDIG =		14
	local DOUBLEQUOTE =			15
	local SINGLEQUOTE =			16

	--Lexemes
	local IDENTIFIER =		1
	local KEYWORD = 		2
	local OPERATOR = 		3
	local NUMERICLITERAL =	4
	local STRINGLITERAL = 	5
	local PUNCTUATION = 	6
	local MALFORMED = 		7

	--Function Variables
	local lexeme
	local pos
	local char
	local state
	local category
	local handlers

	--Returns the current char, or empty string if the end has been reached
    local function currChar()
        return code:sub(pos, pos)
    end

	--Returns the next char, or empty string if the end has been reached
    local function nextChar()
        return code:sub(pos+1, pos+1)
    end

	--Returns 2 chars ahead, only used to determine legitimate scientific notation
    local function next2Char()
        return code:sub(pos+2, pos+2)
    end

	--Advances pos 1 char
    local function drop1()
        pos = pos+1
    end

	--Appends current char to the current lexeme, advances pos 1 char
    local function add1()
        lexeme = lexeme .. currChar()
        drop1()
    end

	--Ignores whitespace and comment notation. Advances pos to the next lexeme
    local function skipSpace()
        while true do
            while isSpace(currChar()) do
                drop1()
            end
			--bypasses /* and everything in between until */ is reached
            if currChar() == "/" and nextChar() == "*" then
                drop1()
                drop1()
                while true do
                    if currChar() == "*" and nextChar() == "/" then
                        drop1()
                        drop1()
                        break
                    elseif currChar() == "" then
                        return
                    else
                        drop1()
                    end
                end
            else
                break
            end
        end
    end

	    -- state handlers
    local function handle_START()
        if isLetter(char) then
            add1()
            state = LETTER
        elseif char == "_" then
            add1()
            state = LETTER
        elseif isDigit(char) then
            add1()
            state = DIGIT
        elseif char == "+" then
            add1()
            state = PLUS
        elseif char == "-" then
            add1()
            state = MINUS
        elseif char == "*" then
            add1()
            state = DONE
            category = OPERATOR
	   elseif char == "." then
            add1()
            state = DOT
        elseif char == "!" then
            add1()
            state = EXCLAMATION
		elseif char == "=" then
            add1()
            state = EQUAL
		elseif char == "<" then
            add1()
            state = LESSTHAN
        elseif char == ">" then
            add1()
            state = GREATERTHAN
        elseif char == "/" then
            add1()
            state = DONE
            category = OPERATOR
	    elseif char == "'" then
            add1()
            state = SINGLEQUOTE
        elseif char == '"' then
            add1()
            state = DOUBLEQUOTE
        elseif char < " " or char > "~" then
            add1()
            state = DONE
            category = MALFORMED
        else
            add1()
            state = DONE
            category = PUNCTUATION
        end
    end

	--Sets up the possibility that this lexeme is scientific notation, will need to 
	--look farther ahead from the SCINOATION state to be sure though. Otherwise handles digits
	local function handle_DIGIT()
        if isDigit(char) then
            add1()
        elseif char == "." then
            add1()
            state = DIGITDOT
        elseif char == "e" or char == "E" then
            state = SCINOTATION
        else
            state = DONE
            category = NUMERICLITERAL
        end
    end

	--Handles digit dots with the possibility that it could be scientific notation
	local function handle_DIGITDOT()
        if isDigit(char) then
            add1()
        elseif char == "e" or char == "E" then
            state = SCINOTATION
        else
            state = DONE
            category = NUMERICLITERAL
        end
    end

	--Handles letters and the 5 reserved keywords
	local function handle_LETTER()
        if isLetter(char) then
            add1()
        elseif char == "_" then
            add1()
        elseif isDigit(char) then
            add1()
        else
            state = DONE
            category = ID
            if  lexeme == "set" 		or	lexeme == "if" or
                lexeme == "printnum" 	or 	lexeme == "printstr" or
                lexeme == "printnl" then
                category = KEY
            end
        end
    end

	local function handle_PLUS()
        if preferOp then
            state = DONE
            category = OPERATOR
            preferOp = false
        elseif isDigit(char) then
            add1()
            state = DIGIT
        elseif char == '.' then
            if isDigit(nextChar()) then
                add1()
                state = DIGITDOT
            else
                state = DONE
                category = OPERATOR
            end
        else
            state = DONE
            category = OPERATOR
        end
    end

    local function handle_MINUS()
        if preferOp then
            state = DONE
            category = OPERATOR
            preferOp = false
        elseif isDigit(char) then
            add1()
            state = DIGIT
        elseif char == '.' then
            if isDigit(nextChar()) then
                add1()
                state = DIGITDOT
            else
                state = DONE
                category = OPERATOR
            end
        else
            state = DONE
            category = OPERATOR
        end
    end

	local function handle_DOT()
        if isDigit(char) then
            add1()
            state = DIGITDOT
        else
            state = DONE
            category = PUNCTUATION
        end
    end

    local function handle_LESSTHAN()
        if char == '=' then
            add1()
            state = DONE
            category = OPERATOR
        else
            state = DONE
            category = OPERATOR
        end
    end

    local function handle_GREATERTHAN()
        if char == '=' then
            add1()
            state = DONE
            category = OPERATOR
        else
            state = DONE
            category = OPERATOR
        end
    end

	local function handle_EXCLAMATION()
        if char == '=' then
            add1()
            state = DONE
            category = OPERATOR
        else
            state = DONE
            category = PUNCTUATION
        end
    end
	
	--Handles the validity of scientific notation
    local function handle_SCINOTATION()
        if nextChar() == "+" or nextChar() == "-" then
            state = SCINOTATIONSIGN
        elseif isDigit(nextChar()) then
            add1()
            add1()
            state = SCINOTATIONDIG
        else
            state = DONE
            category = NUMERICLITERAL
        end
    end

	--Handles the validity of signed scientific notation
    local function handle_SCINOTATIONSIGN()
        if isDigit(next2Char()) then
            add1()
            add1()
            add1()
            state = SCINOTATIONDIG
        else
            state = DONE
            category = NUMERICLITERAL
        end
    end

    local function handle_SCINOTATIONDIG()
        if isDigit(char) then
            add1()
        else
            state = DONE
            category = NUMERICLITERAL
        end
    end

    local function handle_SINGLEQUOTE()
        if char == "" then
            state = DONE
            category = MALFORMED
        elseif char ~= "'" then
            add1()
        else
            add1()
            state = DONE
            category = STRINGLITERAL
        end
    end

    local function handle_DOUBLEQUOTE()
        if char == "" then
            state = DONE
            category = MALFORMED
        elseif char ~= '"' then
            add1()
        else
            add1()
            state = DONE
            category = STRINGLITERAL
        end
    end

    local function handle_EQUAL()
        if char == '=' then
            add1()
            state = DONE
            category = OPERATOR
        else
            state = DONE
            category = OPERATOR
        end
    end

    local function handle_DONE()
        print("ERROR: 'DONE' state should not be handled")
        assert(0)
    end

	handlers =
    {
		[DONE]=handle_DONE,
        [START]=handle_START,
        [LETTER]=handle_LETTER,
        [DIGIT]=handle_DIGIT,
		[PLUS]=handle_PLUS,
        [MINUS]=handle_MINUS,
        [DOT]=handle_DOT,
        [DIGITDOT]=handle_DIGITDOT,
        [EXCLAMATION]=handle_EXCLAMATION,
        [GREATERTHAN]=handle_GREATERTHAN,
        [LESSTHAN]=handle_LESSTHAN,
        [EQUAL]=handle_EQUAL,
		[SCINOTATION]=handle_SCINOTATION,
        [SCINOTATIONSIGN]=handle_SCINOTATIONSIGN,
		[SCINOTATIONDIG]=handle_SCINOTATIONDIG,
		[DOUBLEQUOTE]=handle_DOUBLEQUOTE,
		[SINGLEQUOTE]=handle_SINGLEQUOTE
    }


    local function getLexeme(d1, d2)
        if pos > code:len() then
            preferOp = false
            return nil, nil
        end
        lexeme = ""
        state = START
        while state ~= DONE do
            char = currChar()
            handlers[state]()
        end

        skipSpace()
        preferOp = false
        return lexeme, category
    end

    pos = 1
    skipSpace()
    return getLexeme, nil, nil
end

return lexerit -- return lexit module








