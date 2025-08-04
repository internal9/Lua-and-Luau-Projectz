#!/bin/lua

--[[
	oughta implement code runner and finish parser plsss!
]]

local lexer_parser = require("lexer_parser")
local code_runner = require("code_runner")

local function main()
        local file_name = arg[1]
        assert(file_name ~= nil, "SBL: file name expected")

        local file = io.open(file_name, 'r')
        assert(file ~= nil, "SBL: '".. file_name.. "' is not a valid file")
        assert(string.sub(file_name, #file_name - 3, #file_name) == ".sbl", "SBL: file '".. file_name.. "' must have 'sbl' file extension")

        src_text = file:read("*all")
        local code_parse_tree = lexer_parser(src_text)
end

main()
