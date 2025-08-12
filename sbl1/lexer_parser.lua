--[[
	NOTES:
		GNU Nano displays a tab as multiple columns, while lua treats a tab as just one character
		numbers starting with period shall not be supported for now (eg. ".23")
	FUTURE:
		I should probably add a 'debug env' to allow for type checking during parsing
		(eg. struct a = {}, "a[2]" would be invalid and error)

		Should I replace 'value' in all of the parse_types with more descriptive names?
]]

--[=[
  priority:
  	allow for multi variable declarations (eg. var a, b, c, d)
  	replace lexing same tokens with constant preset tokens
  	
  expressions list:
  	number
  	boolean (aka logical)
]=]

local pretty_table = require("pretty_table")

local fmt = string.format
local tb_insert = table.insert
local str_sub = string.sub

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

-- rename 'refer'? since it's just pass by value
local PARSE_TYPES = {
	FN = "fn",
	FN_CALL = "fn_call",
	INDEX_PATH_ASSIGN = "index_path_assign",
	INDEX_PATH = "index_path",
	STRUCT_INDEX = "struct_index",
	ARRAY_INDEX = "array_index",
	REASSIGN = "reassign",
	IF = "if",
	ELIF = "elif",
	DECLARE = "declare",
	BLOCK = "block",
	BIN_EXPR = "binary_expr",
	UNA_EXPR = "unary_expr",
}

--[[
	implement associativaty, to account for right associative operator '^'
	or just do what C does and have none lol, besides the 'pow' fn provided via 'math.h'
]]
local OP_PRECS = {
	["||"] = 1, ["&&"] = 1,
	["=="] = 2,
	['+'] = 3, ['-'] = 3,
	['*'] = 4, ['/'] = 4,
}

-- will change if '^' is added
local NUM_UNA_PREC = 5
local BOOL_UNA_PREC = 5
		
local tokens = {}
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
local function expect_tk_of_type(type, fail_msg_detail)
	local tk = next_tk()
	assert(tk.type == type,
	  fmt("Invalid %s token '%s' at line %d, column %d. ".. fail_msg_detail, tk.type, tk.value, tk.src_line, tk.src_column))
	return tk
end

-- 'type_debug' solely just to spice up error messages
local function expect_tk_of_value(value, fail_msg_detail)
	local tk = next_tk()
	assert(tk.value == value,
	  fmt("Invalid %s token '%s' at line %d, column %d. ".. fail_msg_detail, tk.type, tk.value, tk.src_line, tk.src_column))
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
	if (is_tk_type_or_value(tk, TK_TYPES.NUM, TK_TYPES.STR, "true", "false", "null")) then
		return tk
		
	elseif (tk.value == '-') then	
		return {type = PARSE_TYPES.UNA_EXPR, op = tk.value, right = parse_expr(NUM_UNA_PREC)}
		
	elseif (tk.value == '(') then
		local expr = parse_expr(0, is_bool_expr)
		expect_tk_of_value(')', "Expected misc token ')' to finish sub-expression.")
		return expr
		
	elseif (tk.type == TK_TYPES.ID) then
		print(tk.value, peek_tk().value)
		return parse_id_value(tk)

	elseif (tk.value == '!') then
		return {type = PARSE_TYPES.UNA_EXPR, op = '!', right = parse_expr(BOOL_UNA_PREC, true)}
	end		
	
	error(fmt("Invalid %s token '%s' at line %d, column %d. Expected number, identifier, 'true', 'false', 'null', '-', or '(' token for parsing expression.",
	  tk.type, tk.value, tk.src_line, tk.src_column))
end

-- would probably be helpful to note that it checks if next tk even exists at all
local function is_next_prec_higher(prec_limit)
	local tk = peek_tk()

	return (tk.type == TK_TYPES.NUM_OP or tk.type == TK_TYPES.BOOL_OP) and (OP_PRECS[tk.value] > prec_limit) or false
end

function parse_expr(prec_limit)
	prec_limit = prec_limit or 0
	is_bool_expr = (is_bool_expr ~= nil) and is_bool_expr or false
	local left_tk = next_tk()
	print_pretty_tb(left_tk)

	assert(left_tk, "Expected number, identifier, '-', or '(' token for parsing expression at..")
	local left = null_denot(left_tk, is_bool_expr)	

	-- temp solution for condition.. maybe not?
	while (is_next_prec_higher(prec_limit)) do
		local op_tk = next_tk()
		local prec = OP_PRECS[op_tk.value]
		local right =  parse_expr(prec)
		
		if (op_tk.type == TK_TYPES.BOOL_OP) then
			left = {type = PARSE_TYPES.BIN_EXPR, left = left, op = op_tk.value, right = right}
		else
			left = {type = PARSE_TYPES.BIN_EXPR, left = left, op = op_tk.value, right = right}
		end
	end

	return left
end

function parse_fn()
	local id_tk = expect_tk_of_type(TK_TYPES.ID, "Expected identifier token for parsing function")
	expect_tk_of_value('(',
	  fmt("Expected misc token '(' for parsing parameters (or none) of function '%s'", id_tk.value))

	local params = {}
	local params_checks = {}
	local expect_param = false

	while (true) do
		local tk = peek_tk()
		
		if (not expect_param) then
			if (tk.value == ')') then 
				tk_index = tk_index + 1
				break
			end
		end

		local param = expect_tk_of_type(TK_TYPES.ID,
		  fmt("Expected an identifier token for parsing parameter of function '%s', or token ')' to close function parameter list",
		   id_tk.value))

		assert(not params_checks[param.value],
		  fmt("Duplicate parameter '%s' at line %d, column %d for function '%s'.",
		   param.value, param.src_line, param.src_column, id_tk.value))
		  
		tb_insert(params, param.value)
		params_checks[param.value] = true
		
		expect_param = false
		
		tk = next_tk()

		if (tk.value == ')') then break end
		assert(tk.value == ',',
		  fmt("Invalid %s token '%s' at line %d, column %d. Expected misc token ',' in function parameter list", tk.type, tk.value, tk.src_line, tk.src_column))
		expect_arg = true
	end

	local block = parse_block()
	expect_tk_of_value("end", fmt("Expected keyword token 'end' to close function '%s'", id_tk.value))

	return {type = PARSE_TYPES.FN, id_name = id_tk.value, params = params, block = block}
end

-- should 'id_tk' param be removed and just read via peek or next tk fn maybe?
function parse_fn_call(id_tk)
	local args = {}
	local expect_arg = false
	
	while (true) do
		local tk = peek_tk()
		
		if (not expect_arg) then
			if (tk.value == ')') then 
				tk_index = tk_index + 1
				break
			end
		end

		local arg = parse_expr()
		expect_arg = false
		
		tb_insert(args, arg)
		tk = next_tk()

		if (tk.value == ')') then break end
		assert(tk.value == ',',
		  fmt("Invalid %s token '%s' at line %d, column %d. Expected misc token ',' in function call argument list or misc token ')' to close function call for function '%s'.",
		  tk.type, tk.value, tk.src_line, tk.src_column, id_tk.value))
		expect_arg = true
	end

	return {type = PARSE_TYPES.FN_CALL, id_name = id_tk.value, args = args}
end

local function parse_elif()
	expect_tk_of_value('(', "Expected misc token '(' to parse elif statement condition.")	  
	local cond = parse_expr(0, true)
	expect_tk_of_value(')', "Expected misc token ')' to close elif statement condition.")

	local block = parse_block()
	return {type = PARSE_TYPES.ELIF, cond = cond, block = block}	
end

local function parse_if()
	expect_tk_of_value('(', "Expected misc token '(' to parse if statement condition.")
	local cond = parse_expr(0, true)
	expect_tk_of_value(')', "Expected misc token ')' to close if statement condition.")

	local block = parse_block()	
	local elif_statements = {}
	local else_block = nil

	while (true) do
		local tk = next_tk()

		if (tk.value == "elif") then
			tb_insert(elif_statements, parse_elif())
			
		elseif (tk.value == "else") then
			else_block = parse_block()
			expect_tk_of_value("end", "Expected keyword token 'end' to close else statement.")
			break
			
		elseif (tk.value == "end") then
			break
		else
			error(fmt("Invalid %s token '%s' at line %d, column %d. Expected 'elif', 'else', or keyword token 'end' to close if statement.",
			  tk.type, tk.value, tk.src_line, tk.src_column))
		end
	end

	return {type = PARSE_TYPES.IF, cond = cond, block = block, elif_statements = elif_statements, else_block = else_block}
end

function form_index_path()
	local index_path = {}
	
 	while (true) do
 		local tk = peek_tk()
 		local index;

		if (tk.value == '.') then
			tk_index = tk_index + 1
			index = expect_tk_of_type(TK_TYPES.ID, "Expected an identifier token for indexing struct.")
			
			tb_insert(index_path, {type = PARSE_TYPES.STRUCT_INDEX, value = index})
							
		elseif (tk.value == '[') then
			tk_index = tk_index + 1
			index = parse_expr()
			
			tb_insert(index_path, {type = PARSE_TYPES.ARRAY_INDEX, value = index})
			expect_tk_of_value(']', "Expected misc token ']' for closing indexing of array.")
		else
			break
		end
 	end

 	return index_path
end

-- bro should i just remove this?
local function parse_null()
	return {type = TK_TYPES.KEYWORD, value = "null"}
end

-- the manual tk_index incrementing is atrocious, never skip planning!
local function parse_var()
	local id_tk = expect_tk_of_type(TK_TYPES.ID, "Expected identifier token to parse var.")

	-- gotta peak so 'tk_index' isn't 2 higher than #tokens
	local tk = peek_tk()
	
	if (tk.value ~= '=') then
		return {type = PARSE_TYPES.DECLARE, id_name = id_tk.value, value = parse_null()}
	end

	tk_index = tk_index + 1
	return {type = PARSE_TYPES.DECLARE, id_name = id_tk.value, value = parse_assign_value()}

	--[[
	--												 unary '-'
	if (is_tk_type_or_value(value_tk, TK_TYPES.NUM, TK_TYPES.ID, TK_TYPES.STR, "null", "true", "false", '!', '-', '(')) then
		return {type = PARSE_TYPES.DECLARE, id_name = id_tk.value, value = parse_expr()}
	end
	
	error(fmt("Invalid %s token '%s', at line %d, column %d. Expected a number, string, identifier, 'true', 'false', '+', '-', or '(' token"..
	  " to assign value for identifier '%s'",
	  value_tk.type, value_tk.value, value_tk.src_line, value_tk.src_column, id_tk.value))
	]]
end

function parse_struct()
	local elements = {}

	-- surely there is a better way to do this entire thing lol?
	if (peek_tk().value == '}') then
		tk_index = tk_index + 1

		-- huh gotos are kinda useful
		goto skip_parse
	end
	
	while (true) do
		local key_tk = expect_tk_of_type(TK_TYPES.ID, "Expected an identifier token for key of struct.")
		
		expect_tk_of_value('=', fmt("Expected misc token '=' for assigning value to element '%s' of struct.",
		  key_tk.value))

		assert(not elements[key_tk.value], fmt("Duplicate element '%s' in struct at line %d, column %d.",
		  key_tk.value, key_tk.src_line, key_tk.src_column))
		  
		elements[key_tk.value] = parse_assign_value()
		
		local tk = next_tk()

		if (tk.value == '}') then break end
		assert(tk.value == ',',
		  fmt("Invalid %s token '%s' at line %d, column %d. Expected misc token ',' in parsing elements of struct or misc token '}' to close struct.",
		  tk.type, tk.value, tk.src_line, tk.src_column))
	end

	::skip_parse::
	return {type = PARSE_TYPES.STRUCT, elements = elements}
end

function parse_array()
	local elements = {}

	-- surely there is a better way to do this entire thing lol?
	if (peek_tk().value == ']') then
		tk_index = tk_index + 1
		goto skip_parse
	end

	while (true) do	
		-- NOTE: index starts at 1 in lua
		tb_insert(elements, parse_assign_value())
		local tk = next_tk()

		if (tk.value == ']') then break end
		assert(tk.value == ',',
		  fmt("Invalid %s token '%s' at line %d, column %d. Expected misc token ',' in parsing elements of array or misc token ']' to close array",
		  tk.type, tk.value, tk.src_line, tk.src_column))
	end

	::skip_parse::
	return {type = PARSE_TYPES.ARRAY, elements = elements}
end

function parse_assign_value()
	local tk = next_tk()
	
	if (tk.value == '{') then
		return parse_struct()
		
	elseif (tk.value == '[') then
		return parse_array()
	end

	-- allow this token to be read by 'parse_expr'
	tk_index = tk_index - 1	
	return parse_expr()	
end

-- parse id when it is the value itself (eg. var b = a)
-- this is only used by 'parse_expr'
function parse_id_value(id_tk)
	local tk = peek_tk()
	
	if (tk.value == '.' or tk.value == '[') then
		return {type = PARSE_TYPES.INDEX_PATH, id_name = id_tk.value, value = form_index_path()}
		  
	elseif (tk.value == '(') then
		tk_index = tk_index + 1
		return parse_fn_call(id_tk)
	end

	-- allow for peeked tk to be read for something else
	return id_tk
end

-- parse id when it's a statement (eg. a = 2, arr[0] = 5)
function parse_id(id_tk)
	local second_tk = next_tk()

	if (second_tk.value == '=') then
		return {type = PARSE_TYPES.REASSIGN, id_name = id_tk.value, value = parse_assign_value()}
		
	elseif (second_tk.type == TK_TYPES.COMP_NUM_OP) then
		local op =  string.sub(second_tk.value, 1, 1)
		local value = {type = PARSE_TYPES.BIN_EXPR, left = id_tk, op = op, right = parse_expr()}
		return {type = PARSE_TYPES.REASSIGN, id_name = id_tk.value, value = value}

	elseif (second_tk.value == '(') then
		return parse_fn_call(id_tk)
		
	elseif (second_tk.value == '.' or second_tk.value == '[') then
		-- allow for '[' or '.' tk to be read by 'form_index_path', ik it's not very 'clean'
		tk_index = tk_index - 1
		local index_path = form_index_path()

		expect_tk_of_value('=', "Expected misc token '=' for assigning value to index path.")
		return {type = PARSE_TYPES.INDEX_PATH_ASSIGN, id_name = id_tk.value, index_path = index_path, value = parse_expr()}
	end

	error(fmt("Invalid %s token '%s' at line %d, column %d. Expected assignment or function call for identifier '%s'.",
	  second_tk.type, second_tk.value, second_tk.src_line, second_tk.src_column, id_tk.value))
end

function parse_statement()
	local tk = next_tk()
	
	if (tk.value == "var") then
		return parse_var()
		
	elseif (tk.value == "if") then
		return parse_if()
		
	elseif (tk.value == "fn") then
		return parse_fn()	
		
	elseif (tk.type == TK_TYPES.ID) then
		return parse_id(tk)
	else
		-- allow for this unmatched token to be read by whatever called this fn (eg. if it's "end", "for", etc)
		tk_index = tk_index - 1
	end
end

-- should i just remove this?
-- edit: NAH
function parse_block()
	local block = {}
	local statement;
	
	repeat
		statement = parse_statement()
		if (not statement) then break end
		  
		tb_insert(block, statement)
	until (peek_tk().value == "EOF")

	return block
end

local function parse_tokens()
	local code_parse_tree = parse_block()
	local tk = next_tk()

	-- if token 'EOF' not reached, it must mean that an invalid token caused 'parse_block' to stop
	assert(tk.value == "EOF", fmt("Invalid %s token '%s' at line %d, column %d. Expected an identifier, 'var', 'fn', or 'if' token",
	  tk.type, tk.value, tk.src_line, tk.src_column))

	return code_parse_tree
end

local function lex_src_text(src_file)
	-- i hate it here, why?
	
	local NUM_OPS = {['+'] = true, ['-'] = true, ['*'] = true, ['/'] = true}	
	local MISC = {['('] = true, [')'] = true, ['.'] = true, [','] = true, ['['] = true,
	  [']'] = true, ['{'] = true, ['}'] = true}
	
	local KEYWORDS = {["var"] = true, ["fn"] = true, ["if"] = true, ["else"] = true,
	  ["elif"] = true, ["end"] = true, [""] = true, ["if"] = true, ["if"] = true,
	  ["true"] = true, ["false"] = true, ["null"] = true}

	local current_line = src_file:read("*line")
	local char_index = 1
	local line_count = 1

	local function lex_num(has_decimal_point)
		local SRC_COLUMN = char_index

		while (true) do
			char_index = char_index + 1
			local char = str_sub(current_line, char_index, char_index)
			
			if (not string.match(char, '%d')) then
				if (char == '.') then
					assert(not has_decimal_point, "Unexpected extra decimal point for number")
					has_decimal_point = true
				else
					local num = str_sub(current_line, SRC_COLUMN, char_index - 1)
					return {type = TK_TYPES.NUM, value = tonumber(num), src_line = line_count, src_column = SRC_COLUMN}
				end
			end
		end
	end

	local function lex_str()
		local SRC_COLUMN = char_index
		local SRC_LINE = line_count
		local str = '"'
		
		while (true) do
			char_index = char_index + 1
			local char = str_sub(current_line, char_index, char_index)
			str = str.. char
			
			if (char == '"') then
				char_index = char_index + 1

				-- //temp
				-- error(str)
				return {type = TK_TYPES.STR, value = str, src_line = SRC_LINE, src_column = SRC_COLUMN}
			end

			if (#current_line == 0 or char_index == #current_line + 1) then
				str = str.. '\n'
				current_line = src_file:read("*line")
				char_index = 0
				line_count = line_count + 1
			end

--			print(line_count, char_index, #current_line, char)
			assert(current_line, fmt("Unclosed string literal beginning at line %d, column %d", SRC_LINE, SRC_COLUMN))
		end
	end

	local function lex_id_or_keyword()
		local SRC_COLUMN = char_index
		
		while (true) do
			char_index = char_index + 1
			local char = str_sub(current_line, char_index, char_index)

			if (not string.match(char, "[%a%d_]")) then
				local value = str_sub(current_line, SRC_COLUMN, char_index - 1)
				local token_type = (KEYWORDS[value]) and TK_TYPES.KEYWORD or TK_TYPES.ID
				
				return {type = token_type, value = value, src_line = line_count, src_column = SRC_COLUMN}
			end
		end
	end

	local function lex_num_op(num_op)
		local SRC_COLUMN = char_index
		char_index = char_index + 1
		local next_char = str_sub(current_line, char_index, char_index)
		
		if (next_char == '=') then
			char_index = char_index + 1
			return {type = TK_TYPES.COMP_NUM_OP, value = num_op.. '=', src_line = line_count, src_column = SRC_COLUMN}
		end

		return {type = TK_TYPES.NUM_OP, value = num_op, src_line = line_count, src_column = SRC_COLUMN}
	end
	
	while (true) do	
		-- why can't lua have 'continue' statement?
		if (#current_line == 0) then goto skip_current_line end
			
		while (char_index ~= #current_line + 1) do
			local char = str_sub(current_line, char_index, char_index)
			
			if (string.match(char, '%s')) then
				char_index = char_index + 1
				
			elseif (string.match(char, '%d')) then
				local HAS_DECIMAL_POINT = false
				tb_insert(tokens, lex_num(HAS_DECIMAL_POINT))

			-- why, i should just form numbers during parsing instead
			elseif (char == '"') then
				tb_insert(tokens, lex_str())
			
			elseif (char == '_' or string.match(char, '%a')) then
				tb_insert(tokens, lex_id_or_keyword())
				
			elseif (MISC[char]) then
				tb_insert(tokens, {type = TK_TYPES.MISC, value = char, src_line = line_count, src_column = char_index})
				char_index = char_index + 1
				
			elseif (NUM_OPS[char]) then
				tb_insert(tokens, lex_num_op(char))

			elseif (char == '!') then
				tb_insert(tokens, {type = TK_TYPES.BOOL_OP, value = '!', src_line = line_count, src_column = char_index})
				char_index = char_index + 1
				-- yuck,wish i had an elegant solution instead of these if statements			
					
			elseif (char == '=') then
				local SRC_COLUMN = char_index
				char_index = char_index + 1

				local next_char = str_sub(current_line, char_index, char_index)

				if (next_char == '=') then
					tb_insert(tokens, {type = TK_TYPES.BOOL_OP, value = "==", src_line = line_count, src_column = SRC_COLUMN})
					char_index = char_index + 1
				else
					tb_insert(tokens, {type = TK_TYPES.MISC, value = '=', src_line = line_count, src_column = SRC_COLUMN})
				end

			elseif (char == '|' or char == '&') then
				local next_char = str_sub(current_line, char_index + 1, char_index + 1)
				local bool_op = char.. char
				assert(next_char == char, fmt("Expected symbol '%s' for lexing boolean operator '%s'", char, char.. char))

				tb_insert(tokens, {type = TK_TYPES.BOOL_OP, value = bool_op, src_line = line_count, src_column = char_index})
				char_index = char_index + 2

			else
				error(fmt("Invalid character '%s' at line %d, column %d", char, line_count, char_index))
			end
			print(char_index, #current_line)
		end

		::skip_current_line::
		current_line = src_file:read("*line")

		if (not current_line) then
			tb_insert(tokens, {type = TK_TYPES.MISC, value = "EOF", src_line = line_count, src_column = char_index})
			break
		end

		char_index = 1
		line_count = line_count + 1
	end
end

return function(src_file)
	-- why
	
	lex_src_text(src_file)
	print_pretty_tb(tokens)
	
	local code_parse_tree = parse_tokens()

	print_pretty_tb(code_parse_tree)
	return code_parse_tree
end
