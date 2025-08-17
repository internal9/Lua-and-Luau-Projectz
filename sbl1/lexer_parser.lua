--[[
	NOTES:
		GNU Nano displays a tab as multiple columns, while lua treats a tab as just one character
		numbers starting with period shall not be supported for now (eg. ".23")
	FUTURE:
		Pls standardize the debugging messages
		
		I should probably add a 'debug env' to allow for type checking during parsing
		(eg. struct a = {}, "a[2]" would be invalid and error)

		Should I replace 'value' in all of the parse_types with more descriptive names?

		How should I implement 'skip' statements?
		EDIT: During running code, when I come across a loop statement, I will save it's index in the parse tree
		which potential said 'skip' statement may use

		Should I 'optimize' multi-line comments? Making it only end if it finds characters '*/' at beginning or end of line?
		I might just check if 'ret' is in function block by checking a call stack for any functions pushed

		Add inc / dec, and '^' ops?
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
local str_match = string.match
local str_sub = string.sub

local function print_pretty_tb(tb) print(pretty_table(tb)) end

-- slightly more detailed for use of debugging messages
local TK_TYPES = require("tk_types")
local PARSE_TYPES = require("parse_types")

--[[
	implement associativaty, to account for right associative operator '^'
	or just do what C does and have none lol, besides the 'pow' fn provided via 'math.h'
]]
local OP_PRECS = {
	["||"] = 1, ["&&"] = 1,
	["=="] = 2, ["!="] = 2,
	["<="] = 3, [">="] = 3, ['<'] = 3, ['>'] = 3,
	['+'] = 4, ['-'] = 4,
	['*'] = 5, ['/'] = 5,
	['?'] = 6,
}

-- will change if '^' is added
local NUM_UNA_PREC = 7
local BOOL_UNA_PREC = 7

-- useful for checking if 'ret' and 'skip' statements are in, or nested in their required blocks
local SCOPES = {
	FN = "fn",
	LOOP = "loop",
	LOOP_IN_FN = "loop_in_fn",
}

local INVALID_TK_MSG = "Invalid %s token '%s' at line %d, column %d. "
		
local tokens = {}
local tk_index = 1

local scope_stack = {}
local scope = nil

local function push_scope(new_scope)
	local prev_scope = scope_stack[#scope_stack]
	
	if ((prev_scope == SCOPES.FN and new_scope == SCOPES.LOOP) or

	   -- must mean it is a nested loop inside a function
	  (prev_scope  == SCOPES.LOOP_IN_FN and new_scope == SCOPES.LOOP)) then
		tb_insert(scope_stack, SCOPES.LOOP_IN_FN)
	else
		tb_insert(scope_stack, new_scope)
	end
	scope = scope_stack[#scope_stack]
end

local function pop_scope()
	scope_stack[#scope_stack] = nil
	scope = scope_stack[#scope_stack]
	print("POP ", scope)
end

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
	  fmt(INVALID_TK_MSG.. fail_msg_detail, tk.type, tk.value, tk.src_line, tk.src_column))
	return tk
end

-- 'type_debug' solely just to spice up error messages
local function expect_tk_of_value(value, fail_msg_detail)
	local tk = next_tk()
	assert(tk.value == value,
	  fmt(INVALID_TK_MSG.. fail_msg_detail, tk.type, tk.value, tk.src_line, tk.src_column))
	return tk
end

-- pratt parsing
local function null_denot(tk)
	if (tk.type == TK_TYPES.NUM or tk.type == TK_TYPES.STR or
	  tk.value == "true" or tk.value == "false" or tk.value == "null") then
		return tk
		
	elseif (tk.value == '-') then	
		return {type = PARSE_TYPES.UNA_EXPR, op_tk = tk, right = parse_expr(NUM_UNA_PREC)}
		
	elseif (tk.value == '(') then
		local expr = parse_expr()
		expect_tk_of_value(')', "Expected misc token ')' to finish sub-expression.")
		return expr
		
	elseif (tk.type == TK_TYPES.ID) then
		return parse_id_value(tk)

	elseif (tk.value == '!') then
		return {type = PARSE_TYPES.UNA_EXPR, op_tk = tk, right = parse_expr(BOOL_UNA_PREC)}
	end		
	
	error(fmt(INVALID_TK_MSG.. "Expected number, identifier, 'true', 'false', 'null', '-', or '(' token for parsing expression.",
	  tk.type, tk.value, tk.src_line, tk.src_column))
end

local function parse_tern_expr(left, op)
	local true_val = parse_value()
	expect_tk_of_value(':', "Expected misc token ':' to prior to parsing false value of ternary operator")
	local false_val = parse_value()
	return {type = PARSE_TYPES.TERN_EXPR, cond = left, false_val = false_val, true_val = true_val}
end

-- would probably be helpful to note that it checks if next tk even exists at all
local function is_next_prec_higher(prec_limit)
	local tk = peek_tk()
	return (tk.type == TK_TYPES.NUM_OP or tk.type == TK_TYPES.BOOL_OP or tk.value == '?')
	  and (OP_PRECS[tk.value] > prec_limit) or false
end

function parse_expr(prec_limit)
	prec_limit = prec_limit or 0
	
	local left_tk = next_tk()
	local left = null_denot(left_tk)

	-- temp solution for condition.. maybe not?
	while (is_next_prec_higher(prec_limit)) do
		local op_tk = next_tk()
		local prec = OP_PRECS[op_tk.value]

		if (op_tk.value == '?') then
			left = parse_tern_expr(left, op)
		else
			left = {type = PARSE_TYPES.BIN_EXPR, left = left, op_tk = op_tk, right = parse_expr(prec)}
		end
	end

	return left
end

-- variadic parameters WILL ALWAYS be after named parameters
function parse_fn(src_line, src_column)
	print("FN CHANGE SCOPE")
	push_scope(SCOPES.FN)
	
	local id_tk = expect_tk_of_type(TK_TYPES.ID, "Expected identifier token for parsing function")
	expect_tk_of_value('(',
	  fmt("Expected misc token '(' for parsing parameters (or none) of function '%s'", id_tk.value))

	local params = {}
	local params_checks = {}
	local expect_param = false
	local has_variadic_params = false

	while (true) do
		local tk = peek_tk()

		if (tk.value == '.') then
			tk_index = tk_index + 1
			
			expect_tk_of_value('.',
			  fmt("Expected two misc tokens '.' for parsing variadic parameters of function '%s'.", id_tk.value))
			expect_tk_of_value('.',
			  fmt("Expected misc token '.' for parsing variadic parameters of function '%s'.", id_tk.value))
			expect_tk_of_value(')',
			  fmt("Expected misc token ')' to close variadic parameters of function '%s'.", id_tk.value))

			has_variadic_params = true
			break
		end

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
		  
		tb_insert(params, param)
		params_checks[param.value] = true
		expect_param = false
		
		tk = next_tk()
		if (tk.value == ')') then break end
		
		assert(tk.value == ',',
		  fmt("Invalid %s token '%s' at line %d, column %d. Expected misc token ',' in function parameter list",
		  tk.type, tk.value, tk.src_line, tk.src_column))
		  
		expect_arg = true
	end

	local block = parse_block(SCOPES.FN)
	expect_tk_of_value("end", fmt("Expected keyword token 'end' to close function '%s' beginning at line %d, column %d.",
	  id_tk.value, src_line, src_column))

	pop_scope()
	return {type = PARSE_TYPES.FN, id_name = id_tk.value, params = params,
	  has_variadic_params = has_variadic_params, block = block,
	  src_line = src_line, src_column = src_column}
end

-- should 'id_tk' param be removed and just read via peek or next tk fn maybe?
function parse_fn_call(id_tk, src_line, src_column)
	local args = {}
	local expect_arg = false
	local has_variadic_args = false
	
	while (true) do
		local tk = peek_tk()
		
		if (not expect_arg) then
			if (tk.value == ')') then 
				tk_index = tk_index + 1
				break
			end
		end

		if (tk.value == '.') then
			tk_index = tk_index + 1

			expect_tk_of_value('.', 
			  fmt("Expected two misc tokens '.' to parse variadic arguments for function call '%s'", id_tk.value))
			expect_tk_of_value('.', 
			  fmt("Expected misc token '.' to parse variadic arguments for function call '%s'", id_tk.value))
			expect_tk_of_value(')',
			  fmt("Expected misc token ')' to close function call '%s'", id_tk.value))
	
			assert(scope == SCOPES.FN or scope == SCOPES.LOOP_IN_FN,
			  fmt("At line %d, column %d. Expected function call '%s' to be inside or nested inside a function block since variadic arguments are present.",
			  tk.src_line, tk.src_column, id_tk.value))
			  
			has_variadic_args = true  
			break
		end
		
		local arg = parse_value()
		expect_arg = false
		
		tb_insert(args, arg)
		tk = next_tk()

		if (tk.value == ')') then break end
		assert(tk.value == ',',
		  fmt(INVALID_TK_MSG.. "Expected misc token ',' in arguments, or misc token ')' to close function call '%s'.",
		  tk.type, tk.value, tk.src_line, tk.src_column, id_tk.value))
		expect_arg = true
	end

	return {type = PARSE_TYPES.FN_CALL, id_name = id_tk.value, args = args,
	  has_variadic_args = has_variadic_args, src_line = src_line, src_column = src_column}
end

local function parse_elif()
	expect_tk_of_value('(', "Expected misc token '(' to parse elif statement condition.")	  
	local cond = parse_expr()
	expect_tk_of_value(')', "Expected misc token ')' to close elif statement condition.")

	local block = parse_block()
	return {type = PARSE_TYPES.ELIF, cond = cond, block = block}	
end

-- scope may be 'fn_block', which in that case it allows for return statements in here
local function parse_if()
	expect_tk_of_value('(', "Expected misc token '(' to parse if statement condition.")
	local cond = parse_expr()
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
			error(fmt(INVALID_TK_MSG.. "Expected elif statement, else statement, or keyword token 'end' to close if statement.",
			  tk.type, tk.value, tk.src_line, tk.src_column))
		end
	end

	return {type = PARSE_TYPES.IF, cond = cond, block = block, elif_statements = elif_statements, else_block = else_block}
end

local function parse_case()
	local case_exprs = {}
	local block;
	tb_insert(case_exprs, parse_expr())

	while (peek_tk().value == ',') do
		tk_index = tk_index + 1
		tb_insert(case_exprs, parse_expr())
	end

	expect_tk_of_value(':', "Expected misc token ':' to parse case block")
	local IS_IN_CASES = true
	return {case_exprs = case_exprs, block = parse_block(IS_IN_CASES)}
end

function parse_cases()
	expect_tk_of_value('(', "Expected misc token '(' prior to parsing cases statement argument expression")
	local value = parse_expr()
	
	expect_tk_of_value(')', "Expected misc token ')' to close cases statement argument expression")
	local cases = {}
	local none_block = nil

	-- awful code, not the best
	-- duplicate cases will be checked at run-time, since these are expression
	while (true) do
		tb_insert(cases, parse_case())

		local tk = peek_tk()

		if (tk.value == "none") then
			tk_index = tk_index + 1
			expect_tk_of_value(':', "Expected misc token ':' to parse case 'none' block")
			none_case_block = parse_block()
			
			expect_tk_of_value("end", "Expected statement(s), or keyword token 'end' to close cases statement")
			break
		end

		if (tk.value == "end") then
			tk_index = tk_index + 1
			break
		end

		assert(tk.value ~= "EOF",
		  fmt("Invalid misc token 'EOF' at line %d, column %d. Expected cases(s), or keyword token 'end' to close cases statement",
		  tk.src_line, tk.src_column))
	end

	return {type = PARSE_TYPES.CASES, value = value, cases = cases, none_case_block = none_case_block}
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
local function parse_var(src_line, src_column)
	local id_tk = expect_tk_of_type(TK_TYPES.ID, "Expected identifier token to parse var.")

	-- gotta peak so 'tk_index' isn't 2 higher than #tokens
	local tk = peek_tk()
	
	if (tk.value ~= '=') then
		return {type = PARSE_TYPES.DECLARE, id_name = id_tk.value, value = parse_null(),
		  src_line = src_line, src_column = src_column}
	end

	tk_index = tk_index + 1
	return {type = PARSE_TYPES.DECLARE, id_name = id_tk.value, value = parse_value(),
	  src_line = src_line, src_column = src_column}
end

function parse_struct()
	-- surely there is a better way to do this entire thing lol?
	if (peek_tk().value == '}') then
		tk_index = tk_index + 1
		return {type = PARSE_TYPES.STRUCT, elements = {}}
	end

	local elements = {}

	while (true) do
		local key_tk = expect_tk_of_type(TK_TYPES.ID, "Expected an identifier token for key of struct.")
		
		expect_tk_of_value('=', fmt("Expected misc token '=' for assigning value to element '%s' of struct.",
		  key_tk.value))

		assert(not elements[key_tk.value], fmt("Duplicate element '%s' in struct at line %d, column %d.",
		  key_tk.value, key_tk.src_line, key_tk.src_column))
		  
		elements[key_tk.value] = parse_value()
		
		local tk = next_tk()

		if (tk.value == '}') then break end
		assert(tk.value == ',',
		  fmt(INVALID_TK_MSG.. "Expected misc token ',' in parsing struct elements or misc token '}' to close struct.",
		  tk.type, tk.value, tk.src_line, tk.src_column))
	end

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
		tb_insert(elements, parse_value())
		local tk = next_tk()

		if (tk.value == ']') then break end
		assert(tk.value == ',',
		  fmt(INVALID_TK_MSG.. "Expected misc token ',' in parsing array elements or misc token ']' to close array",
		  tk.type, tk.value, tk.src_line, tk.src_column))
	end

	::skip_parse::
	return {type = PARSE_TYPES.ARRAY, elements = elements}
end

-- parsing a value that is usually on the right side of a declaration or initialization
function parse_value()
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
		local src_line = id_tk.src_line
		local src_column = id_tk.src_column
		
		return parse_fn_call(id_tk, src_line, src_column)
	end

	-- allow for peeked tk to be read for something else
	return id_tk
end

-- parse id when it's a statement (eg. a = 2, arr[0] = 5)
function parse_id(id_tk, src_line, src_column)
	local second_tk = next_tk()
	
	if (second_tk.value == '=') then
		return {type = PARSE_TYPES.REASSIGN, id_name = id_tk.value,
		  value = parse_value(), src_line = src_line, src_column = src_column}
		
	elseif (second_tk.type == TK_TYPES.COMP_NUM_OP) then
		local op =  string.sub(second_tk.value, 1, 1)
		second_tk.type = TK_TYPES.NUM_OP
		second_tk.value = op
		local value = {type = PARSE_TYPES.BIN_EXPR, left = id_tk, op_tk = second_tk, right = parse_expr()}
		return {type = PARSE_TYPES.REASSIGN, id_name = id_tk.value, value = value}

	elseif (second_tk.value == '(') then
		return parse_fn_call(id_tk, src_line, src_column)
		
	elseif (second_tk.value == '.' or second_tk.value == '[') then
		-- allow for '[' or '.' tk to be read by 'form_index_path', ik it's not very 'clean'
		tk_index = tk_index - 1
		local index_path = form_index_path()

		expect_tk_of_value('=', "Expected misc token '=' for assigning value for index path.")
		return {type = PARSE_TYPES.INDEX_PATH_ASSIGN, id_name = id_tk.value, index_path = index_path, value = parse_expr()}
	end

	error(fmt(INVALID_TK_MSG.. "Expected assignment or function call for identifier '%s'.",
	  second_tk.type, second_tk.value, second_tk.src_line, second_tk.src_column, id_tk.value))
end

function parse_ret()
	local tk = peek_tk()
	local value = nil

	if (tk.value == "false" or tk.value == "true" or tk.value == "null" or
	  tk.type == TK_TYPES.NUM or tk.type == TK_TYPES.STR or tk.type == TK_TYPES.ID or
	  tk.value == '(' or tk.value == '[' or tk.value == '{') then
	  	value = parse_value()
	else
		value = parse_null()
	end
	
	return {type = PARSE_TYPES.RET, value = value}
end

-- scope refers to what scope the statement is in
function parse_for()
	push_scope(SCOPES.LOOP)
	
	-- awful error messags
	expect_tk_of_value('(', "Expected misc token '(' prior to parsing for loop statement arguments.")
	local index_id = expect_tk_of_type(TK_TYPES.ID, "Expected an identifier token to be for loop statement index identifier.")
	
	expect_tk_of_value(',', "Expected misc token ',' prior to parsing for loop statement index start expression.")
	local start = parse_expr()

	expect_tk_of_value(',', "Expected misc token ',' prior to parsing for loop statement index limit expression.")

	-- expression's result will be typechecked for a number
	local limit = parse_expr()
	expect_tk_of_value(',', "Expected misc token ',' prior to parsing for loop statement index increment expression.")

	local increment = parse_expr()
	expect_tk_of_value(')', "Expected misc token ')' to close for loop statement expressions.")

	local block = parse_block()
	expect_tk_of_value("end", "Expected keyword token 'end' to close for loop statement block.")

	pop_scope()
	return {type = PARSE_TYPES.FOR, index_id = index_id.value, limit = limit, increment = increment, block = block}
end

function parse_iter()
	push_scope(SCOPES.LOOP)
	
	expect_tk_of_value('(', "Expected misc token '(' to parse iter loop statement arguments.")

	-- will be typechecked for a struct or array	
	local value = parse_value()
	expect_tk_of_value(',', "Expected misc token ',' prior to parsing iter loop statement index identifier.")

	local index_id = expect_tk_of_type(TK_TYPES.ID, "Expected an identifier token to be iter loop statement index identifier.")
	expect_tk_of_value(',', "Expected misc token ',' prior to parsing iter loop statement element identifier.")
	
	local element_id = expect_tk_of_type(TK_TYPES.ID, "Expected an identifier token to be iter loop statement element identifier.")

	expect_tk_of_value(')', "Expected misc token ')' to close iter loop statement arguments.")

	local block = parse_block()
	expect_tk_of_value("end", "Expected keyword token 'end' to close iter loop statement block.")

	pop_scope()
	return {type = PARSE_TYPES.ITER, value = value, index_id = index_id.value, element_id = element_id.value, block = block}
end

function parse_while()
	push_scope(SCOPES.LOOP)
	
	expect_tk_of_value('(', "Expected misc token '(' prior to parsing while loop statement condition.")
	local cond = parse_expr()
	expect_tk_of_value(')', "Expected misc token ')' to close while loop statement condition.")

	local block = parse_block()
	expect_tk_of_value("end", "Expected keyword token 'end' to close while loop statement block.")

	pop_scope()
	return {type = PARSE_TYPES.WHILE, cond = cond, block = block}
end

function parse_rep()
	push_scope(SCOPES.LOOP)
	print(scope)
	local block = parse_block(scope)
	
	expect_tk_of_value("until", "Expected keyword token 'until' to prior to parsing repeat loop condition.")

	expect_tk_of_value('(', "Expected misc token '(' to parse repeat loop statement condition.")
	local cond = parse_expr()
	expect_tk_of_value(')', "Expected misc token ')' to close repeat loop statement condition.")

	pop_scope()
	print(scope)
	return {type = PARSE_TYPES.REP, cond = cond, block = block}
end

-- scope may be 'nil'
-- 'is_in_cases', not the most elegant solution, but it prevents an identifier from erroring if the tk after it is a ':', thus it is a case
function parse_statement(is_in_cases)
	local tk = next_tk()
	local src_line = tk.src_line
	local src_column = tk.src_column
	
	if (tk.value == "var") then
		return parse_var(src_line, src_column)
		
	elseif (tk.value == "if") then
		return parse_if()
		
	elseif (tk.value == "cases") then
		return parse_cases()
		
	elseif (tk.value == "fn") then
		return parse_fn(src_line, src_column)
		
	elseif (tk.type == TK_TYPES.ID) then
		local second_tk = peek_tk()
		
		if (is_in_cases and (second_tk.value == ':' or second_tk.value == ',')) then
			-- allow identifier to be read by 'parse_case'
			tk_index = tk_index - 1
			return
		end
		return parse_id(tk, src_line, src_column)
		
	elseif (tk.value == "ret") then
		assert(scope == SCOPES.FN or scope == SCOPES.LOOP_IN_FN,
		  fmt("Expected return statement at line %d, column %d to be a function block or in nested loops inside a function block.", tk.src_line, tk.src_column))  
		return parse_ret(tk)
	
	elseif (tk.value == "while") then
		return parse_while()
		
	elseif (tk.value == "rep") then
		return parse_rep()
			
	elseif (tk.value == "for") then
		return parse_for()
		
	elseif (tk.value == "iter") then
		return parse_iter()
			
	elseif (tk.value == "skip") then
		assert(scope == SCOPES.LOOP or scope == SCOPES.LOOP_IN_FN,
		  fmt("Expected skip statement at line %d, column %d to be inside loop block.", tk.src_line, tk.src_column))  

		-- ya idrk what to do here
		return {type = PARSE_TYPES.SKIP}
		
	elseif (tk.value == "break") then
		assert(scope == SCOPES.LOOP or scope == SCOPES.LOOP_IN_FN,
		  fmt("Expected break statement at line %d, column %d to be inside loop block.", tk.src_line, tk.src_column))  

		-- ya idrk what to do here
		return {type = PARSE_TYPES.BREAK}
	else
		-- allow for this unmatched token to be read by whatever called this fn (eg. if it's "end", "for", etc)
		tk_index = tk_index - 1
	end
end

function parse_block(is_in_cases)
	local block = {}
	local statement;
	
	repeat
		statement = parse_statement(is_in_cases)
		if (not statement) then break end

		tb_insert(block, statement)

		-- no statements are allowed under a 'skip' or 'ret' statement
		if (statement.type == PARSE_TYPES.RET or statement.type == PARSE_TYPES.SKIP) then
			break
		end
		
	until (peek_tk().value == "EOF")

	return block
end

local function parse_tokens()
	local code_parse_tree = parse_block()
	local tk = next_tk()
	
	-- if token 'EOF' not reached, it must mean that an invalid token caused 'parse_block' to stop
	assert(tk.value == "EOF", fmt("Invalid %s token '%s' at line %d, column %d. Expected a statement, function declaration, or function call",
	  tk.type, tk.value, tk.src_line, tk.src_column))

	return code_parse_tree
end

local function lex_src_text(src_file)
	-- i hate it here, why?
	
	local NUM_OPS = {['+'] = true, ['-'] = true, ['*'] = true, ['/'] = true, ['%'] = true}
	local BOOL_OPS = {['!'] = true, ['<'] = true, ['>'] = true}
	local MISC = {['('] = true, [')'] = true, ['.'] = true, [','] = true, ['['] = true,
	  [']'] = true, ['{'] = true, ['}'] = true, [':'] = true, ['?'] = true}
	
	local KEYWORDS = {["var"] = true, ["fn"] = true, ["if"] = true, ["elif"] = true,
	  ["else"] = true, ["end"] = true, ["ret"] = true, ["skip"] = true,
	  ["for"] = true, ["while"] = true, ["iter"] = true, ["rep"] = true, ["until"] = true,
	  ["true"] = true, ["false"] = true, ["null"] = true,
	  ["cases"] = true, ["none"] = true, ["break"] = true}

	local current_line = src_file:read("*line")
	local char_index = 1
	local line_count = 1
	local eof_src_column;

	local function next_line()
		current_line = src_file:read("*line")
		-- yuck
		if (not current_line) then eof_src_column = char_index end 

		char_index = 1
		line_count = line_count + 1
	end
	
	local function lex_num(has_decimal_point)
		local SRC_COLUMN = char_index

		while (true) do
			char_index = char_index + 1
			local char = str_sub(current_line, char_index, char_index)
			
			if (not str_match(char, '%d')) then
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
		local str = ''

		char_index = char_index + 1
		
		while (true) do
			local char = str_sub(current_line, char_index, char_index)
			
			if (char == '"') then
				char_index = char_index + 1
				return {type = TK_TYPES.STR, value = str, src_line = SRC_LINE, src_column = SRC_COLUMN}
			end
			
			str = str.. char

			if (#current_line == 0 or char_index == #current_line + 1) then
				str = str.. '\n'
				next_line()
				assert(current_line, fmt("Unclosed string literal beginning at line %d, column %d", SRC_LINE, SRC_COLUMN))
			else
				char_index = char_index + 1
			end

--			print(line_count, char_index, #current_line, char)
--			char_index = char_index + 1
		end
	end

	local function lex_id_or_keyword()
		local SRC_COLUMN = char_index
		
		while (true) do
			char_index = char_index + 1
			local char = str_sub(current_line, char_index, char_index)

			if (not str_match(char, "[%a%d_]")) then
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

	local function lex_bool_op(bool_op)
		local SRC_COLUMN = char_index
		char_index = char_index + 1
		local next_char = str_sub(current_line, char_index, char_index)
		
		if (next_char == '=') then
			bool_op = bool_op.. '='
			char_index = char_index + 1
		end

		return {type = TK_TYPES.BOOL_OP, value = bool_op, src_line = line_count, src_column = SRC_COLUMN}
	end

	local function handle_comment()
		local SRC_LINE = line_count
		local SRC_COLUMN = char_index
		
		char_index = char_index + 1
		local char = str_sub(current_line, char_index, char_index)

		if (char == '/') then
			-- entire line is excluded from lexing
			char_index = #current_line + 1
			
		elseif (char == '*') then
			char_index = char_index + 1
			-- this is awful
			
			while (current_line) do				
				while (char_index < #current_line + 1) do
					local char = str_sub(current_line, char_index, char_index)

					if (char == '*') then
						char_index = char_index + 1
						char = str_sub(current_line, char_index, char_index)

						if (char == '/') then
							char_index = char_index + 1
							return
						end
					end
					char_index = char_index + 1
				end
								
				next_line()
			end

			error(fmt("Unclosed multi-line comment beginning at line %d, column %d. Expected '*/' on any line together to close said comment", SRC_LINE, SRC_COLUMN))
		end
	end

	local function is_comment()
		local char = str_sub(current_line, char_index + 1, char_index + 1)
		return char == '/' or char == '*'
	end

	-- yeah i'm literally just gonna rewrite this with if statements instead lol
	while (current_line) do	
		-- why can't lua have 'continue' statement?
		-- if (#current_line == 0) then goto skip_current_line end

		-- NOTE: empty lines with no characters have a len of '0'			
		while (char_index < #current_line + 1) do
			local char = str_sub(current_line, char_index, char_index)
			
			if (str_match(char, '%s')) then
				char_index = char_index + 1

			-- not my proudest code
			elseif (str_match(char, '/') and is_comment()) then
				handle_comment(char == '*')
			
			elseif (str_match(char, '%d')) then
				local HAS_DECIMAL_POINT = false
				tb_insert(tokens, lex_num(HAS_DECIMAL_POINT))

			-- why, i should just form numbers during parsing instead
			elseif (char == '"') then
				tb_insert(tokens, lex_str())
			
			elseif (char == '_' or str_match(char, '%a')) then
				tb_insert(tokens, lex_id_or_keyword())
				
			elseif (MISC[char]) then
				tb_insert(tokens, {type = TK_TYPES.MISC, value = char, src_line = line_count, src_column = char_index})
				char_index = char_index + 1
				
			elseif (NUM_OPS[char]) then
				tb_insert(tokens, lex_num_op(char))

			elseif (BOOL_OPS[char]) then
				tb_insert(tokens, lex_bool_op(char))

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

		next_line()
	end

	-- 'linecount - 1' since 'current_line' is nil
	tb_insert(tokens, {type = TK_TYPES.MISC, value = "EOF", src_line = line_count - 1, src_column = eof_src_column})
end

return function(src_file)
	-- why
	
	lex_src_text(src_file)
	print_pretty_tb(tokens)
	
	local code_parse_tree = parse_tokens()

	print_pretty_tb(code_parse_tree)
	print("FINAL SCOPE: ", scope)
	return code_parse_tree
end
