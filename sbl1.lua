--[=[
  priority:
  	implement better debugging
  	finish if statements
  	replace lexing same tokens with constant preset tokens
  	
  expressions list:
  	number
  	boolean (aka logical)
]=]

local pretty_table = require("pretty_table")

local fmt = string.format
local function print_pretty_tb(tb) print(pretty_table(tb)) end

-- slightly more detailed for use of debugging messages
local TK_TYPES = {
	KEYWORD = "keyword",
	ID = "identifier",
	STR = "string",
	NUM = "number",
	BOOL = "boolean",
	NUM_OP = "number operator",
	BOOL_OP = "boolean operator",
	COMP_NUM_OP = "compound number operator",
	BOOL_OP = "boolean operator",
	MISC = "misc",
}

local PARSE_TYPES = {
	FN = "fn",
	FN_CALL = "fn_call",
	STRUCT_INDEX = "struct_index",
	IF = "if",
	ELIF = "elif",
	DECLARE = "declare",
	BLOCK = "block",
	LITERAL = "literal",
	ID_REFER = "id_refer",
	NUM_BIN_EXPR = "number_binary_expr",
	NUM_UNA_EXPR = "number_unary_expr",
	BOOL_BIN_EXPR = "bool_binary_expr",
	BOOL_UNA_EXPR = "bool_unary_expr"
}

-- implement associativaty, to account for right associative operator '^'
local OP_PRECS = {
	["||"] = 1, ["&&"] = 1,
	["=="] = 2,
	['+'] = 3, ['-'] = 3,
	['*'] = 4, ['/'] = 4,
}

-- will change if '^' is added
local NUM_UNA_PREC = 6
local BIN_UNA_PREC = 6
		
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
	
	assert(tk, fmt("Expected %s token", type))
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
local function null_denot(tk, is_bool_expr)
	if (tk.type == TK_TYPES.NUM) then
		return {type = PARSE_TYPES.LITERAL, value = tk.value}
		
	elseif (tk.value == '-') then	
		return {type = PARSE_TYPES.NUM_UNA_EXPR, op = tk.value, right = parse_expr(NUM_UNA_PREC)}
		
	elseif (tk.value == '(') then
		local expr = parse_expr(0, is_bool_expr)
		expect_tk_of_value(TK_TYPES.MISC, ')')
		return expr
		
	elseif (tk.type == TK_TYPES.ID) then
		return parse_id(tk)

	elseif (tk.value == '!') then
		return {type = PARSE_TYPES.BOOL_UNA_EXPR, op = '!', right = parse_expr(BIN_UNA_PREC, true)}
			
	elseif (tk.value == "true" or tk.value == "false" or tk.value == "null" or tk.type == TK_TYPES.STR) then
		return {type = PARSE_TYPES.LITERAL, value = tk.value}

	end		
	
	error(fmt("Invalid %s token '%s'. Expected number, identifier, '-', or '(' token for parsing expression", tk.type, tk.value))
end

-- would probably be helpful to note that it checks if next tk even exists at all
local function is_next_prec_higher(prec_limit)
	local tk = peek_tk()

	if (not tk) then return false end
	return (tk.type == TK_TYPES.NUM_OP or tk.type == TK_TYPES.BOOL_OP) and (OP_PRECS[tk.value] > prec_limit) or false
end

function parse_expr(prec_limit)
	prec_limit = prec_limit or 0
	is_bool_expr = (is_bool_expr ~= nil) and is_bool_expr or false
	local left_tk = next_tk()

	assert(left_tk, "Expected number, identifier, '-', or '(' token for parsing expression at..")
	local left = null_denot(left_tk, is_bool_expr)	

	-- temp solution for condition.. maybe not?
	while (is_next_prec_higher(prec_limit)) do
		local op_tk = next_tk()
		local prec = OP_PRECS[op_tk.value]
		local right =  parse_expr(prec)
		
		if (op_tk.type == TK_TYPES.BOOL_OP) then
			left = {type = PARSE_TYPES.BOOL_BIN_EXPR, left = left, op = op_tk.value, right = right}
		else
			left = {type = PARSE_TYPES.NUM_BIN_EXPR, left = left, op = op_tk.value, right = right}
		end
	end

	return left
end

function parse_fn_call(id_tk)
	
end

local function parse_if_clause()
	expect_tk_of_value(TK_TYPES.MISC, '(')	
	local cond = parse_expr(0, true)
	expect_tk_of_value(TK_TYPES.MISC, ')')

	local block = parse_block()
	return {type = PARSE_TYPES.ELIF, cond = cond, block = block}	
end

local function parse_if()
	expect_tk_of_value(TK_TYPES.MISC, '(')	
	local cond = parse_expr(0, true)
	expect_tk_of_value(TK_TYPES.MISC, ')')
	
	local block = {}
	local elif_statements = {}
	local else_block = nil

	print_pretty_tb(cond)

	while (true) do
		if (tk_index == #tokens + 1) then
			-- forgot to close if statement block
		end
		
		local statement = parse_statement()
		
		if (not statement) then
			local tk = next_tk()
			assert(tk, "Expected keyword token 'end' or statements")

			if (tk.value == "elif") then
				table.insert(elif_statements, parse_if_clause())
				
			elseif (tk.value == "else") then
				else_block = parse_block()

				-- temporary debug lol
				-- print("else: ", pretty_table(else_block))
				-- print("SDSSDSDSD", tokens[tk_index], tk_index, #tokens)
				expect_tk_of_value(TK_TYPES.KEYWORD, "end")

				break
				
			elseif (tk.value == "end") then
				break
				
			else
				error(fmt("Invalid %s token '%s', starting at index %d. Expected an identifier, 'var', 'fn', 'if' token, or 'end'  token to close block",
			  	  tk.type, tk.value, tk.src_start_index))
			end
		end

		table.insert(block, statement)
	end

	return {type = PARSE_TYPES.IF, cond = cond, block = block, elif_statements = elif_statements, else_block = else_block}
end

-- TODO: add function call

-- return id_ref instead of 'id_tk' itself?
function parse_id(id_tk)
	local tk = peek_tk()
	if (not tk) then return {type = PARSE_TYPES.ID_REFER, id_name = id_tk.value} end

	if (tk.value == '.') then

		-- support multiple indexing (eg. var x = a.b.c.d)
		local index_tk = peek_tk(1)

		assert(index_tk, fmt("Expected identifier token for indexing struct %s", id_tk.value))
		assert(index_tk.type == TK_TYPES.ID,
		  fmt("Invalid %s token '%s'. Expected identifier token for indexing struct %s", index_tk.type, index_tk.value, id_tk.value))

		tk_index = tk_index + 2	  
		return {type = PARSE_TYPES.ID_REFER, id_name = id_tk.value, index_name = index_tk.value}
		
	elseif (tk.value == '(') then
		tk_index = tk_index + 1
		return parse_fn_call(id_tk)
	end

	return id_tk
end

local function parse_null()
	return {type = PARSE_TYPES.LITERAL, value = "null"}
end

local function parse_var()
	local id_tk = expect_tk_of_type(TK_TYPES.ID)

	-- gotta peak so 'tk_index' isn't 2 higher than #tokens
	local tk = peek_tk()
	
	if (not tk or tk.value ~= '=') then
		return {type = PARSE_TYPES.ID_REFER, id_name = id_tk.value, value = parse_null()}
	end

	local value_tk = peek_tk(1)
	tk_index = tk_index + 1
	
	assert(value_tk, "Expected a number, string, identifier, 'null', '+', '-', or '(' token")
	local value;

	-- unary '+' and '-'
	if (is_tk_type_or_value(value_tk, TK_TYPES.NUM, TK_TYPES.ID, "true", "false", '!', '-', '(')) then
		value = parse_expr()
		
	elseif (value_tk.type == TK_TYPES.STR) then
		value = parse_str()
		
	elseif (value_tk.value == "null") then
		tk_index = tk_index + 1
		value = parse_null()
	
	else
		error(fmt("Invalid %s token '%s', starting at index %d. Expected a number, string, identifier, 'true', 'false', '+', '-', or '(' token",
		  value_tk.type, value_tk.value, value_tk.src_start_index))

	end

	return {type = PARSE_TYPES.VAR, id_name = id_tk.value, value = value}
end

local function parse_assign()

end

function parse_statement()
	local tk = next_tk()
	
	if (tk.value == "var") then
		return parse_var()
		
	elseif (tk.value == "if") then
		return parse_if()
		
	elseif (tk.value == "fn") then
		-- parse_fn()	
		
	elseif (tk.type == TK_TYPES.ID) then
		return parse_assign()

		-- Not my proudest code
	else
		tk_index = tk_index - 1
	end
end

-- should i just remove this?
function parse_block()
	local block = {}
	local statement;
	
	repeat
		statement = parse_statement()
		if (not statement) then break end
		  
		table.insert(block, statement)
	until (tk_index == #tokens + 1)

	return block
end

local function parse_tokens()
	local parse_tree = {}
	local statement;
	
	repeat
		statement = parse_statement()
		
		if (not statement) then
			local tk = tokens[tk_index]
			
			error(fmt("Invalid %s token '%s', starting at index %d. Expected an identifier, 'var', 'fn', or 'if' token",
			  tk.type, tk.value, tk.src_start_index))
		end
		  
		table.insert(parse_tree, statement)
	until (tk_index == #tokens + 1)
	
	print_pretty_tb(parse_tree)
end

local function lex_src_text()
	local char_index = 1
	local current_str = ""

	-- i hate it here, why?
	
	local NUM_OPS = {['+'] = true, ['-'] = true, ['*'] = true, ['/'] = true}	
	local MISC = {['('] = true, [')'] = true, ['.'] = true}
	
	local KEYWORDS = {["var"] = true, ["fn"] = true, ["if"] = true, ["else"] = true,
	  ["elif"] = true, ["end"] = true, [""] = true, ["if"] = true, ["if"] = true,
	  ["true"] = true, ["false"] = true, ["null"] = true}

	local function lex_num(has_decimal_point)
		local NUM_START_INDEX = char_index

		while (true) do
			char_index = char_index + 1
			local char = string.sub(src_text, char_index, char_index)
			
			if (not string.match(char, '%d')) then
				if (char == '.') then
					assert(not has_decimal_point, "Unexpected extra decimal point for number")
					has_decimal_point = true
				else
					local num = string.sub(src_text, NUM_START_INDEX, char_index - 1)
					return {type = TK_TYPES.NUM, value = tonumber(num), src_start_index = NUM_START_INDEX}
				end
			end

			current_str = current_str.. char
		end
	end

	local function lex_str()
		local STR_START_INDEX = char_index
		
		while (true) do
			char_index = char_index + 1
			local char = string.sub(src_text, char_index, char_index)
			
			if (char == '"') then
				local str = string.sub(src_text, STR_START_INDEX, char_index)
				char_index = char_index + 1
				return {type = TK_TYPES.STR, value = str, src_start_index = STR_START_INDEX}
			end
			
			assert(char_index ~= #src_text, "Unclosed string literal")
		end
	end

	local function lex_id_or_keyword()
		local VALUE_START_INDEX = char_index
		
		while (true) do
			char_index = char_index + 1
			local char = string.sub(src_text, char_index, char_index)

			if (not string.match(char, "[%a%d_]")) then
				local value = string.sub(src_text, VALUE_START_INDEX, char_index - 1)
				local token_type = (KEYWORDS[value]) and TK_TYPES.KEYWORD or TK_TYPES.ID
				
				return {type = token_type, value = value, src_start_index = VALUE_START_INDEX}
			end
		end
	end

	local function lex_num_op(num_op)
		char_index = char_index + 1
		local next_char = string.sub(src_text, char_index, char_index)
		
		if (next_char == '=') then
			return TK_TYPES.COMP_NUM_OP, num_op.. '='
		else
			return TK_TYPES.NUM_OP, num_op
		end
	end

	repeat
		local char = string.sub(src_text, char_index, char_index)
		
		if (string.match(char, '%s')) then
			char_index = char_index + 1
			
		elseif (string.match(char, '%d')) then
			local HAS_DECIMAL_POINT = false
			table.insert(tokens, lex_num(HAS_DECIMAL_POINT))

		elseif (char == '.') then
			local HAS_DECIMAL_POINT = true
			table.insert(tokens, lex_num(HAS_DECIMAL_POINT))

		elseif (char == '"') then
			table.insert(tokens, lex_str())
		
		elseif (char == '_' or string.match(char, '%a')) then
			table.insert(tokens, lex_id_or_keyword())
			
		elseif (MISC[char]) then
			table.insert(tokens, {type = TK_TYPES.MISC, value = char, src_start_index = char_index})
			char_index = char_index + 1
			
		elseif (NUM_OPS[char]) then
			local tk_type, num_op = lex_num_op(char)
			table.insert(tokens, {type = tk_type, value = num_op, src_start_index = char_index})

		elseif (char == '!') then
			table.insert(tokens, {type = TK_TYPES.BOOL_OP, value = '!', src_start_index = char_index})
			char_index = char_index + 1
			-- yuck,wish i had an elegant solution instead of these if statements			
				
		elseif (char == '=') then
			char_index = char_index + 1

			local next_char = string.sub(src_text, char_index, char_index)

			if (next_char == '=') then
				table.insert(tokens, {type = TK_TYPES.BOOL_OP, value = "==", src_start_index = char_index})
				char_index = char_index + 1
			else
				table.insert(tokens, {type = TK_TYPES.MISC, value = '=', src_start_index = char_index})
			end

		elseif (char == '|' or char == '&') then
			local next_char = string.sub(src_text, char_index + 1, char_index + 1)
			local bool_op = char.. char
			assert(next_char == char, fmt("Expected symbol '%s' for lexing boolean operator '%s'", char, char.. char))

			table.insert(tokens, {type = TK_TYPES.BOOL_OP, value = bool_op, src_start_index = char_index})
			char_index = char_index + 2

		else
			error(fmt("Invalid symbol '%s' at .. [DEBUGGING NOT HERE YET]", char))
		end

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
        print(print_pretty_tb(tokens))

	parse_tokens(tokens)
end

main()
