#!/bin/lua

--[[
	TODO: finish parsing and implement execution
	TODO: add expressions, and support for unary minus and plus to replace the sign thingy in parsing numbers
	
	add local env inside functions, thx!
	
	we may have scopes defining what type of statements can be parsed
	(eg. no parse var allowed inside for loop scope)
	
	prototyping 1:
	i = 2
	for (i = 0, 5, 10) -- parse number vars inside parse for loop
		print(i)
	end
	array = [2,2,2]	-- parse var and table
	print (array[1])	-- * zero index based

	prototyping 2:
	op_precs = [
		+, - = 1,
		*, / = 2;
	]
	
	fn parse_expr expr_tokens()
		current_token_prec = null

		fn split_tokens_by_op tokens()
			for (i = #tokens - 1, -1)
				token = expr_tokens[i]	
				if (token.type != "op")
					skip
				end

				current_token_prec = op_precs[token.value]
				if (current_token_prec == 1)
					ret
				end
			end
		end
	end
	
]]

--SBL: super basic language
local prettytable = require("prettytable")

local oper_precs = {
	['+'] = 1,
	['-'] = 1,
	['*'] = 2,
	['/'] = 2,
}
--[[
	["operator"] = true,
	["keyword"] = true,
	["string"] = true,
	["constant"] = true,	
]]

-- exceptions include: identiifer, string, number as they aren't fixed
local tokens_lookup = {
	['='] = "assignment",
	
	['+'] = "operator",
	['-'] = "operator",
	['*'] = "operator",
	['/'] = "operator",

	['('] = "parenthesis",
	[')'] = "parenthesis",
	[','] = "delimiter",

	["for"] = "keyword",
	["fn"] = "keyword",
	["end"] = "keyword",

	-- why do we even have types for square brackets?
	['['] = "square_bracket",
	[']'] = "square_bracket",

	['"'] = "double_quote",

}

-- restricting what parse types are allowed in a scope (eg. only parse_assignment allowed in for loop)
-- marked for deletion prob
local keyword_parse_funcs = {
	["for"] = function(line_tokens)

	end,

	["if"] = function(line_tokens)

	end
}


-- possibly marked for deletion
local parse_funcs = {
	["keyword"] = function(token_value, ...)
		return keyword_parse_funcs[token_value](...)
	end,

	["identifier"] = function(name, token, ...)
		print(...)
	end,

	
	
}
----

-- Runtime & built in vars and funcs
local env = {
	vars = {},
	funcs = {	},
}

-- util parse tuple?
function util_parse_tuple(parse_type_to_log, identifier_name_to_log, packed_tokens)
	local parsed_elements = {}
	local is_parsing_element = true
	local index = 0
	
	while true do
		index = index + 1
		local token = packed_tokens[index]
		
		if is_parsing_element then
			local element;
			
			if token.type == "number" then
				element = token
				
			elseif token.value == '+' or token.value == '-' then
				index = index + 1
				local token_2 = packed_tokens[index]
				element = parse_expression(packed_tokens)
				
			else
				error("Invalid token: '".. token.value.. "' for parsing token of ".. parse_type_to_log.. " '" .. identifier_name_to_log.. "'")
			end
			
			table.insert(parsed_elements, element)
		else
			assert(token.value == ',', "Expected delimiter ',' for parsing ".. parse_type_to_log.. " '".. identifier_name_to_log.. "'")
		end
		
		if index == #packed_tokens then
			assert(token.value ~= ',', "Unexpected delimiter ',' at parsing ".. parse_type_to_log.. " '".. identifier_name_to_log.. "'")
			break
		end
		
		is_parsing_element = not is_parsing_element
	end

	return parsed_elements
end

-- remove sign and instead make this an expression? (such as unary minus)
--[[
function parse_number(sign, token_1, ...)	
	assert(... == nil, "Too many arguments for parsing number '".. tostring(token_1.value).. "'")
	
	assert(token_1.type == "number" or token_1.type == "identifier", "Expected 'number' or 'identifier' for parsing number")

	local parse_info = {
		type = "number",
		sign = sign,
		value = token_1
	}

	return parse_info
end
]]

function parse_string(...)
	local packed_tokens = {...}
	local last_token = table.remove(packed_tokens, #packed_tokens)
	
	assert(last_token.type == "double_quote", "Unclosed string literal")

	local str_value = ''

	for _, token in ipairs(packed_tokens) do
		str_value = str_value.. token.value
	end

	local parse_info = {
		type = "string",
		value = str_value
	}
	
	return parse_info	
end

-- support functions, etc, etc
function parse_func_call(identifier, ...)
	local packed = {...}
	local last_token = table.remove(packed, #packed)
	assert(last_token.value == ')', "Expected closing ')'  for parsing function call '".. identifier.value.. "'")

	local PARSE_TYPE_TO_LOG = "function call"
	local IDENTIFIER_NAME_TO_LOG = identifier.value

	local func_call_parse_info = {
		type = "function_call",
		name = identifier.value,
		args = #packed ~= 0 and util_parse_tuple(PARSE_TYPE_TO_LOG, IDENTIFIER_NAME_TO_LOG, packed) or nil
	}

	return func_call_parse_info
end

function parse_table(identifier, ...)
	local packed = {...}
	local last_token = table.remove(packed, #packed)
	assert(last_token ~= nil and last_token.value == ']', "Expected closing ']' for parsing table '".. identifier.value.. "'")

	local PARSE_TYPE_TO_LOG = "table"
	local IDENTIFIER_NAME_TO_LOG = identifier.value
	
	local table_parse_info = {
		type = "table",
		elements = #packed ~= 0 and util_parse_tuple(PARSE_TYPE_TO_LOG, IDENTIFIER_NAME_TO_LOG, packed) or {}
	}
	
	return table_parse_info
end

-- probably split it up into "paths"
function parse_assignment(identifier, token_1, ...)
	local value;

	if token_1.type == "number" then
		value = token_1
		
	elseif token_1.value == "+" or token_1.value == "-" then
		value = parse_expression(token_1, ...)
		
	elseif token_1.type == "double_quote" then
		value = parse_string(...)

	elseif token_1.type == "identifier" then
		-- either reference or function call

		local token_2 = select(1, ...)

		if token_2 ~= nil then
			-- either function call or table indexing
			
			if token_2.value == '(' then
				value = parse_func_call(token_1, select(2, ...))
				
			elseif token_2.type == '[' then
				value = parse_table_index()
			end
		else
			value = parse_reference(token_1, ...)		
		end
				
	elseif token_1.value == "[" then
		value = parse_table(identifier, ...)
	else
		error("Invalid token '".. token_1.value.. "' for parsing assignment")
	end
	
	local parse_info = {
		type = "assignment",
		name = identifier.value,
		value = value,
	}

	return parse_info
end

function path_parse_identifier(identifier, token_1, ...)
	local name = identifier.value

	for i = 1, #name do
		local char = string.sub(name, i, i)
		assert(string.match(char, "[_%a%d]"),
		 "Identifier '".. name.. "' should only contain underscores, numbers, or letters") 
	end

	assert(token_1 ~= nil, "Expected statement or function call for parsing identifier '".. identifier.value.. "'")
	
	if token_1.value == "(" then
		return parse_func_call(identifier, ...)
	elseif token_1.value == "=" then
		return parse_assignment(identifier, ...)
	else

	end
end

local function parse_tokens(tokens)
	local parsed_statements = {}
	
	for _, line_tokens in ipairs(tokens) do
		local token_1 = line_tokens[1]
		
		if token_1.type == "keyword" then
			local parser_func = keyword_parse_funcs[token_1.value]

			if parser_func ~= nil then
				-- parser_func(table.unpack(line_tokens))
			end
			
		elseif token_1.type == "identifier" then
			local debug_pretty_table_str = prettytable(path_parse_identifier(table.unpack(line_tokens)))
			print(debug_pretty_table_str)
		else
			print("SBL (ERR): invalid token ".. token_1.value)
			break
		end
	end

	return parsed_statements
end

local function tokenize(text)
	local tokens = {}


	-- possible usage for determining strings before parsing?
	local in_double_quotes = false

	local function make_identifier_token(str)
		local token = {
			type = tonumber(str) and "number" or "identifier",
			value = str,
		}
		
		return token
	end
	
	for line in string.gmatch(text, "[^\n]+") do
		local line_tokens = {}
		table.insert(tokens, line_tokens)

		local token
		local start_index, end_index = 1, 1
		local last_str = nil
		
		while true do
			::token_cont::

			print(start_index, end_index)
			
			if end_index == #line + 1 then
				if last_str ~= nil then
					local token;
					token = make_identifier_token(last_str)
					table.insert(line_tokens, token)
				end
				
				break
			end
			
			local char = string.sub(line, end_index, end_index)

			-- btw newline gets counted
			if char:match("%s") then
				local token;
				
				start_index = end_index + 1
				end_index = start_index
				
				if last_str ~= nil then
					token = make_identifier_token(last_str)
					table.insert(line_tokens, token)
					last_str = nil
				end

				if in_double_quotes then
					token = {
						type = "string_whitespace",
						value = char
					} 
					table.insert(line_tokens, token)
				end
			else
				local str = string.sub(line, start_index, end_index)
				local token_type;
				local value;
					
				if tokens_lookup[char] ~= nil then
					token_type = tokens_lookup[char]
					value = char

					if token_type == "double_quote" then
						in_double_quotes = not in_double_quotes
					end

					if last_str ~= nil then
						token = make_identifier_token(last_str)
						table.insert(line_tokens, token)
						last_str = nil
					end
				elseif tokens_lookup[str] ~= nil then
					token_type = tokens_lookup[str]
					value = str
					last_str = nil
				else		
					last_str = str
					end_index = end_index + 1

					-- seriously, just add continue statements
					goto token_cont
				end

				token = {
					type = token_type,
					value = value,
				}
				table.insert(line_tokens, token)

				start_index = end_index + 1
				end_index = start_index
			end
		end
	end

	return tokens
end

local function main()
	local file_name = arg[1]
	assert(file_name ~= nil, "SBL: file name expected")
	assert(string.sub(file_name, #file_name - 3, #file_name) == ".sbl", "SBL: file '".. file_name.. "' must have 'sbl' file extension")

	local file = io.open(file_name, 'r')
	assert(file ~= nil, "SBL: '".. file_name.. "' is not a valid file")
	
	local file_text = file:read("*all")
	local start = os.clock()
	local tokens = tokenize(file_text)
	print(prettytable(tokens))

	local parsed_statements = parse_tokens(tokens)
end	

main()
