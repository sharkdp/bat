--- Finds factorial of a number.
-- @param value Number to find factorial.
-- @return Factorial of number.
local function factorial(value)
    if value <= 1 then
        return 1
    else
        return value * factorial(value - 1)
    end
end

--- Joins a table of strings into a new string.
-- @param table Table of strings.
-- @param separator Separator character.
-- @return Joined string.
local function join(table, separator)
    local data = ""
    
    for index, value in ipairs(table) do
        data = data .. value .. separator
    end
    
    data = data:sub(1, data:len() - 1)
    
    return data
end

local a = factorial(5)

print(a)

local b = join({ "l", "u", "a" }, ",")

print(b)
