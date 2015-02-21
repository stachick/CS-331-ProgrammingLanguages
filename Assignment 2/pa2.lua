--pa2.lua
--Shane Tachick
--CS 331 - Programming Languages
--February 12, 2015
--Homework 2


--Exercise A

--initialize module
local pa2 = {}

function pa2.concatLimit (str, desiredSize)
	--temp string to concatenate into, provides an
	--empty string if the length of str is greater
	--than the desired size
	local tempStr = ""
	--getting the length of the string
	local strLength = string.len(str)
	--loop doing all the work. i starts at the length
	--of the string and increments by the length of
	--the string until the desired size is reached/exceeded
	for i = strLength, desiredSize, strLength do
		tempStr = tempStr..str
	end
	return tempStr
end


function pa2.mapTable (f, t)
	--applying the function to the values in the table
	for key, value in pairs(t) do
		t[key] = f(value)
	end
	return t
end


function pa2.collatzSeq (k)
	while true do
		--passes out the calculated value
		coroutine.yield(k)
		--ends when k is 1
		if k == 1 then
			break
		end
		--if even, k/2
		if k%2 == 0 then
			k = k/2
		--if odd, 3k+1
		else
			k = (3*k) + 1
		end
	end
end


return pa2
--return module



--Exercise B
--I am certain that the haskell program is spamming the president
--with threatening emails (you fiend!), however the only immediatly
--visible functionality is a printed message that reads:
--"Here's some Haskell. Cool, eh?"
