--[=[
  gotta implement parsing, floating points
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

-- parse tree node types
local ND_TYPES = {
	VAR = "var",
	NUM = "num",
}

-- implement associativaty, to account for right associative operator '^'
local NUM_OP_PRECS = {
	['+'] = 1, ['-'] = 1,
	['*'] = 1, ['/'] = 2,	
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
local function expect_tk(type, value)
	local tk = next_tk()
	
	if (value) then
		-- param 'type' is assumed
		assert(tk.value == value, fmt("Invalid %s token '%s', expected %s token '%s'", tk.type, tk.value, type, value))
	else
		assert(tk.type == type, fmt("Invalid %s token '%s', expected %s token", tk.type, tk.value, type))
	end
	
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
		return {type = "unary", op = tk.value, right = parse_expr()}
		
	elseif (tk.type == TK_TYPES.ID) then
		
	else
		error(fmt("Expected number, identifier, '-', or '(' token for parsing expression. Instead got %s token '%s'", tk.type, tk.value))
	end
end

function parse_expr(prec_limit)
	prec_limit = prec_limit or 0
	
	local left_tk = next_tk()
	assert(left_tk, "Expected number, identifier, '-', or '(' token for parsing expression")
	
	local left = null_denot(left_tk)

	-- temp solution for condition
	while (peek_tk() and NUM_OP_PRECS[peek_tk().value] and NUM_OP_PRECS[peek_tk().value] > prec_limit) do
		local op_tk = next_tk()
		local prec = NUM_OP_PRECS[op_tk.value]
		
		left = {type = "binary", left = left, op = op_tk.value, right = parse_expr(prec)}		
	end

	return left
end

-- TODO: add function call
local function parse_id(id_tk)
	local tk = peek_tk(1)

	if (tk and tk.value == '.') then
		local index_tk = peek_tk(2)

		assert(index_tk, fmt("Expected identifier token for indexing struct %s", id_tk.value))
		assert(index_tk.type == TK_TYPES.ID,
		  fmt("Invalid %s token '%s'. Expected identifier token for indexing struct %s", index_tk.type, index_tk.value, id_tk.value))

		tk_index = tk_index + 3		  
		return {type = "struct_index", id_name = id_tk.value, index_name = index_tk.value}
	else
		return parse_expr()
	end
end

local function parse_var()	
	local id_tk = expect_tk(TK_TYPES.ID, nil)
	expect_tk(TK_TYPES.MISC, '=')
	local value_tk = peek_tk()

	assert(value_tk, "Expected a number, string, identifier, '+', '-', or '(' token")
	local value;
	
	if (value_tk.type == TK_TYPES.ID) then
		-- parse a ref, struct index, or expr
		value = parse_id(value_tk)

	-- unary '+' and '-'
	elseif (is_tk_type_or_value(value_tk, TK_TYPES.NUM, '+', '-', '(')) then
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
		
			while (true) do
				char_index = char_index + 1
				char = string.sub(src_text, char_index, char_index)
				
				if (not string.match(char, '%d')) then
					local num = string.sub(src_text, NUM_START_INDEX, char_index - 1)
					table.insert(tokens, { type = TK_TYPES.NUM, value = num, src_start_index = NUM_START_INDEX })
					break
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
					error("unclosed string literal")
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
			table.insert(tokens, { type = TK_TYPES.MISC, value = char, src_text_start_index = char_index })
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
