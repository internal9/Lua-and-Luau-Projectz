--[=[
  gotta implement parsing, floating points, logging messages accounting for lines
]=]

local pretty_table = require("prettytable")

local fmt = string.format
local function pretty_tb_print(tb) print(pretty_table(tb)) end

local TK_TYPES = {
	KEYWORD = "keyword",
	ID = "identifier",
	STR = "string",
	NUM = "number",
	NUM_OP = "number operator",
	COMP_NUM_OP = "compound number operator",
	BOOL_OP = "boolean operator",
	MISC = "misc",
}

local PARSE_TYPES = {
	FN = "fn",
	FN_CALL = "fn_call",
	STRCT_INDEX = "struct_index",
	
}

-- parse tree node types
local ND_TYPES = {
	VAR = "var",
	NUM = "num",
	BIN = "binary",
	UNA = "unary",
}

-- implement associativaty, to account for right associative operator '^'
local NUM_OP_PRECS = {
	['+'] = 1, ['-'] = 1,
	['*'] = 2, ['/'] = 2,	
}

local src_text;

local tokens = {}
local parse_tree = {}

local tk_index = 1

local function next_tk()
	local tk = tokens[tk_index]
	tk_index = tk_index + 1
	return tk
end

local function peek_tk(offset_index)
	offset_index = offset_index or 0
	local tk = tokens[tk_index + offset_index]
	return tk
end

-- mode for 'peek' or 'next'?
-- FIXME you 
local function expect_tk_of_type(type)
	local tk = next_tk()
	assert(tk.type == type, fmt("Invalid %s token '%s', expected %s token", tk.type, tk.value, type))
	return tk
end

-- 'type_debug' solely just to spice up error messages
local function expect_tk_of_value(type_debug, value)
	local tk = next_tk()

	assert(tk, fmt("Expected %s token '%s'", type_debug, value))
	assert(tk.value == value, fmt("Invalid %s token '%s'. Expected %s token '%s'", tk.type, tk.value, type_debug, value))
	
	return tk
end

local function is_tk_type(tk, ...)
	for _, type in ipairs({...}) do
		if (tk.type == type) then return true end
	end
	
	return false
end

local function is_tk_type_or_value(tk, ...)
	for _, type_or_value in ipairs({...}) do
		if (tk.type == type_or_value or tk.value == type_or_value) then return true end
	end
	
	return false
end

-- pratt parsing
local function null_denot(tk)
	if (tk.type == TK_TYPES.NUM) then
		return tk
		
	elseif (tk.value == '-') then
	
		-- will change if '^' is added
		local UNARY_PREC = 3
		return {type = ND_TYPES.UNA, op = tk.value, right = parse_expr(UNARY_PREC)}

	elseif (tk.value == '(') then
		local expr = parse_expr()
		expect_tk_of_value(TK_TYPES.MISC, ')')
		return expr
		
	elseif (tk.type == TK_TYPES.ID) then
		return parse_id(tk)
	else
		error(fmt("Invalid %s token '%s'. Expected number, identifier, '-', or '(' token for parsing expression", tk.type, tk.value))
	end
end

local function is_next_prec_higher(prec_limit)
	local tk = peek_tk()
	if (not tk) then return false end
	return tk.type == TK_TYPES.NUM_OP and (NUM_OP_PRECS[tk.value] > prec_limit) or false
end

function parse_expr(prec_limit)
	prec_limit = prec_limit or 0
	local left_tk = next_tk()
	assert(left_tk, "Expected number, identifier, '-', or '(' token for parsing expression at..")
	
	local left = null_denot(left_tk)	

	-- temp solution for condition.. maybe not?
	while (is_next_prec_higher(prec_limit)) do
		local op_tk = next_tk()
		local prec = NUM_OP_PRECS[op_tk.value]
		
		left = {type = ND_TYPES.BIN, left = left, op = op_tk.value, right = parse_expr(prec)}		
	end

	return left
end

function parse_fn_call(id_tk)
	
end

-- TODO: add function call
function parse_id(id_tk)
	local tk = peek_tk()
	if (not tk) then return {type = "id_ref", id_name = id_tk.value} end

	pretty_tb_print(tk)	
	if (tk.value == '.') then

		-- support multiple indexing (eg. var x = a.b.c.d)
		local index_tk = peek_tk(1)

		assert(index_tk, fmt("Expected identifier token for indexing struct %s", id_tk.value))
		assert(index_tk.type == TK_TYPES.ID,
		  fmt("Invalid %s token '%s'. Expected identifier token for indexing struct %s", index_tk.type, index_tk.value, id_tk.value))

		tk_index = tk_index + 2	  
		return {type = "struct_index", id_name = id_tk.value, index_name = index_tk.value}
		
	elseif (tk.value == '(') then
		tk_index = tk_index + 1
		return parse_fn_call(id_tk)
	end

	return {type = "id_ref", id_name = id_tk.value}
end

local function parse_var()	
	local id_tk = expect_tk_of_type(TK_TYPES.ID)
	expect_tk_of_value(TK_TYPES.MISC, '=')
	local value_tk = peek_tk()

	assert(value_tk, "Expected a number, string, identifier, '+', '-', or '(' token")
	local value;

	-- unary '+' and '-'
	if (is_tk_type_or_value(value_tk, TK_TYPES.NUM, TK_TYPES.ID, '+', '-', '(')) then
		value = parse_expr()
		
	elseif (value_tk.type == TK_TYPES.STR) then
		value = parse_str()
		
	else
		error(fmt("Invalid %s token '%s', starting at index %d. Expected a number, string, identifier, '+', '-', or '(' token",
		  value_tk.type, value_tk.value, value_tk.src_start_index))

	end
	
	return {type = ND_TYPES.VAR, id_name = id_tk.value, value =  value}
end

local function parse_if()

end

local function parse_tokens()
	local statement;
	
	while (true) do
		local tk = next_tk()
		if (not tk) then break end

		if (tk.value == "var") then
			statement = parse_var()

		elseif (tk.value == "fn") then
			-- parse_fn()
						
		elseif (tk.value == "if") then
			statement = parse_if()
			
		elseif (tk.type == TK_TYPES.ID) then
			statement = parse_assign()
		else
			error(fmt("Invalid %s token '%s', starting at index %d. Expected an identifier, 'var', 'fn', or 'if' token",
			  tk.type, tk.value, tk.src_start_index))
		end

		table.insert(parse_tree, statement)
	end

	pretty_tb_print(parse_tree)
end

local function lex_src_text()
	local char_index = 1
	local current_str = ""

	-- i hate it here

	local NUM_OPS = {['+'] = true, ['-'] = true, ['*'] = true, ['/'] = true}
	
	local MISC = {['='] = true, ['!'] = true, ['('] = true, [')'] = true, ['.'] = true}
	
	local KEYWORDS = {["var"] = true, ["fn"] = true, ["if"] = true, ["else"] = true,
	  ["eif"] = true, ["end"] = true, [""] = true, ["if"] = true, ["if"] = true,}


	repeat
		local char = string.sub(src_text, char_index, char_index)
		print(char_index)
		if (string.match(char, '%s')) then
			char_index = char_index + 1
			
		elseif (string.match(char, '%d')) then
			local NUM_START_INDEX = char_index
			local has_decimal_point = false		

			while (true) do
				char_index = char_index + 1
				char = string.sub(src_text, char_index, char_index)
				
				if (not string.match(char, '%d')) then
					if (char == '.') then
						assert(not has_decimal_point, "Unexpected extra decimal point for number")
						has_decimal_point = true
					else
						local num = string.sub(src_text, NUM_START_INDEX, char_index - 1)
						table.insert(tokens, { type = TK_TYPES.NUM, value = tonumber(num), src_start_index = NUM_START_INDEX })
						break
					end
				end

				current_str = current_str.. char
			end

		elseif (char == '"') then
			local STR_START_INDEX = char_index
			
			while (true) do
				char_index = char_index + 1
				char = string.sub(src_text, char_index, char_index)
				
				if (char == '"') then
					local str = string.sub(src_text, STR_START_INDEX, char_index)
					table.insert(tokens, { type = TK_TYPES.STR, value = str, src_start_index = STR_START_INDEX })
					
					char_index = char_index + 1
					break
				end
					
				if (char_index == #src_text) then
					error("Unclosed string literal")
				end
			end
		
		elseif (char == '_' or string.match(char, '%a')) then
			local VALUE_START_INDEX = char_index
			
			while (true) do
				char_index = char_index + 1
				char = string.sub(src_text, char_index, char_index)

				if (not string.match(char, "[%a%d_]")) then
					local value = string.sub(src_text, VALUE_START_INDEX, char_index - 1)
					local token_type = (KEYWORDS[value]) and TK_TYPES.KEYWORD or TK_TYPES.ID
					
					table.insert(tokens, { type = token_type, value = value, src_start_index = VALUE_START_INDEX })
					break
				end
			end

		elseif (MISC[char]) then
			table.insert(tokens, { type = TK_TYPES.MISC, value = char, src_start_index = char_index })
			char_index = char_index + 1
			
		elseif (NUM_OPS[char]) then
			char_index = char_index + 1
			local next_char = string.sub(src_text, char_index, char_index)
			local tk_type, op;
			
			if (next_char == '=') then
				tk_type = TK_TYPES.COMP_NUM_OP
				op = char.. '='
			else
				tk_type = TK_TYPES.NUM_OP
				op = char
			end
			
			table.insert(tokens, { type = tk_type, value = op, src_text_start_index = char_index })
		else
			error(fmt("Invalid symbol %s", char))
		end

--		print(char_index == #src_text, char_index, #src_text)
	until (char_index == #src_text)
end

local function main()
        local file_name = arg[1]
        assert(file_name ~= nil, "SBL: file name expected")

        local file = io.open(file_name, 'r')
        assert(file ~= nil, "SBL: '".. file_name.. "' is not a valid file")
        assert(string.sub(file_name, #file_name - 3, #file_name) == ".sbl", "SBL: file '".. file_name.. "' must have 'sbl' file extension")

        src_text = file:read("*all")
        
        lex_src_text()
        print(pretty_tb_print(tokens))

	parse_tokens(tokens)
end

main()
