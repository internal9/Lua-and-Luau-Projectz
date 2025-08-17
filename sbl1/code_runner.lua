local pretty_table = require("pretty_table")
local function print_pretty_tb(tb) print(pretty_table(tb)) end

local fmt = string.format

local TK_TYPES = require("tk_types")
local PARSE_TYPES = require("parse_types")
local env = {}

-- built in globals
local function G_log(...)
	local evaluated = {}
	for index, arg in ipairs({...}) do
		evaluated[index] = arg.value
	end
	print(table.unpack(evaluated))
end

-- log global environment
local function G_log_genv(section)
	if (section) then
		assert(section.type == TK_TYPES.STR and (section.value == "fns" or section.value == "vars"))
		print_pretty_tb(env[section.value])
	else
		print_pretty_tb(env)
	end
end

-- stuff
env.vars = {}
env.fns = {
	log = {run = G_log, is_built_in = true},
	log_genv = {run = G_log_genv, is_built_in = true}
		
}

local call_stack = {}
local index_stack = {}
local loop_stack = {{}}

local nested_block_count = 0
local is_fn_returning = false
local is_loop_breaking = false
local is_loop_skipping = false
local fn_ret_val = {type = "keyword", value = "null"}

local function push_index()
	index_stack[#index_stack + 1] = 0
end

local function pop_index()
	index_stack[#index_stack] = nil
end

local function get_index()
	return index_stack[#index_stack]
end

local function inc_index()
	index_stack[#index_stack] = index_stack[#index_stack] + 1
end

local function dec_index()
	index_stack[#index_stack] = index_stack[#index_stack] - 1
end

-- util
-- will not handle tables as keys
local function deep_clone_tb(tb)
	local clone_tb = {}
	for k, v in pairs(tb) do
		clone_tb[k] = (type(v) == "table") and deep_clone_tb(v) or v
	end
	return clone_tb
end

local function assert_tree_type(tree, error_msg, ...)
	for _, type in ipairs({...}) do
		if (tree.type) == type then break end
	end
	error(error_msg)
end

local function is_null(val)
	return (val.type == TK_TYPES.KEYWORD) and val.value == "null"
end

local function is_expr(val)
	return (val.type == PARSE_TYPES.UNA_EXPR or val.type == PARSE_TYPES.BIN_EXPR) or
	false
end

local function is_literal(val)
	return (val.type == TK_TYPES.STR or val.type == TK_TYPES.NUM or
	  ((val.type == TK_TYPES.KEYWORD) and (val.value == "true" or val.value == "false")) or is_null(val)) or
	  false
end

-- truthy is nything besides null or false
local function is_falsy(val)
	return is_null(val) or (val.type == TK_TYPES.KEYWORD and val.value == "false")
end

local function cond_run_block(cond, block)
	local evalued = eval_value(cond)
	local value = evalued.value

	if (not is_falsy(evalued)) then
		run_block(block)
		return true
	end
	return false
end

local function eval_tern_expr(cond, true_val, false_val)
	local evaluated = eval_value(cond)
	true_val = eval_value(true_val)
	false_val = eval_value(false_val)

	if (is_falsy(evaluated)) then
		return false_val
	end
	return true_val
end

local function eval_str_expr(left, op_tk, right)
	assert(op_tk.value == '+',
	  fmt("Invalid %s operator '%s' at line %d, column %d, can only concatenate strings with operator '+'.",
	  op_tk.type, op_tk.value, op_tk.src_line, op_tk.src_column))

	assert(right.type == TK_TYPES.STR,
	  fmt("Cannot concatenate string '%s' with %s '%s' at line %d, column %d.",
	  left.value, right.type, right.value, op_tk.src_line, op_tk.src_column))
	  
	return {type = TK_TYPES.STR, value = left.value.. right.value,
	  src_line = left.src_line, src_column = left.src_column}
end

local function eval_bool_expr(left, op_tk, right)
	local op = op_tk.value
	local value;

	if (op == "==") then
		assert(left.type == right.type or is_null(left) or is_null(right),
		  fmt("Cannot compare if %s '%s' is equal to %s '%s' at line %d, column %d.",
		  left.type, left.value, right.type, right.value, op_tk.src_line, op_tk.src_column))

		value = (left.type == right.type and left.value == right.value) and "true" or "false"
		return {type = TK_TYPES.KEYWORD, value = value,
		  src_line = left.src_line, src_column = left.src_column}
	elseif (op == "!=") then
		assert(left.type == right.type or is_null(left) or is_null(right),
		  fmt("Cannot compare if %s '%s' not equal to %s '%s' at line %d, column %d.",
		  left.type, left.value, right.type, right.value, op_tk.src_line, op_tk.src_column))
		  
		value = (left.type ~= right.type or left.value ~= right.value) and "true" or "false"
		return {type = TK_TYPES.KEYWORD, value = value,
		  src_line = left.src_line, src_column = left.src_column}
	elseif (op == '!') then
		error("ASDSAD")
	end
		  
	-- is num op
	assert(left.type == TK_TYPES.NUM and right.type == TK_TYPES.NUM,
	  fmt("Cannot perform boolean expression with %s '%s' with %s '%s' at line %d, column %d, expected both to be numbers.",
	  left.type, left.value, right.type, right.value, op_tk.src_line, op_tk.src_column))	
	  
	if (op == '>') then
		value = (left.value > right.value) and "true" or "false"
	elseif (op == '<') then
		value = (left.value < right.value) and "true" or "false"
	elseif (op == ">=") then
		value = (left.value >= right.value) and "true" or "false"
	elseif (op == "<=") then
		value = (left.value <= right.value) and "true" or "false"
	end

	return {type = TK_TYPES.KEYWORD, value = value,
	  src_line = left.src_line, src_column = left.src_column}
end

local function eval_una_expr(parse_tree)
	local op_tk = parse_tree.op_tk
	local op = op_tk.value
	local right = eval_value(parse_tree.right)

	if (op == '!') then
		local value = is_falsy(right) and "true" or "false"
		return {type = TK_TYPES.KEYWORD, value = value,
		  src_line = op_tk.src_line, src_column = op_tk.src_column}
		
	elseif (op == '-') then
		assert(right.type == TK_TYPES.NUM,
		  fmt("Cannot negate %s '%s' at line %d, column %d, only numbers are allowed",
		  right.type, right.value, right.src_line, right.src_column))
		return {type = TK_TYPES.NUM, value = -right.value,
		  src_line = op_tk.src_line, src_column = op_tk.src_column}
	end
end

function eval_expr(parse_tree)
	if (parse_tree.type == PARSE_TYPES.UNA_EXPR) then
		return eval_una_expr(parse_tree)		
	end
	
	local left = eval_value(parse_tree.left)
	local right = eval_value(parse_tree.right)
	local op_tk = parse_tree.op_tk

	if (left.type == TK_TYPES.STR and op_tk.value == '+') then
		return eval_str_expr(left, op_tk, right)
	end

	if (op_tk.type == TK_TYPES.BOOL_OP) then
		return eval_bool_expr(left, op_tk, right)
	end
	
	assert(left.type == TK_TYPES.NUM,
	  fmt("Cannot do arithmetic operation '%s' on %s '%s' with %s '%s' at line %d, column %d.",
	  op_tk.value, left.type, left.value, right.type, right.value, op_tk.src_line, op_tk.src_column))
	  
	assert(right.type == TK_TYPES.NUM,
	  fmt("Cannot do arithmetic operation on number '%d' with %s '%s' at line %d, column %d.",
	  left.value, right.type, right.value, op_tk.src_line, op_tk.src_column))

	local op = op_tk.value
	local left_val = left.value
	local right_val = right.value
	local value;

	if (op == '+') then
		value = left_val + right_val
	elseif (op == '-') then
		value = left_val - right_val
	elseif (op == '*') then
		value = left_val * right_val
	elseif (op == '/') then
		value = left_val / right_val
	elseif (op == '%') then
		value = left_val % right_val
	end

	return {type = TK_TYPES.NUM, value = value, src_line = left.src_line, src_column = left.src_column}
end

local function arr_to_debug_str(elements)
	local str = '['
	for index, element in ipairs(elements) do
		if (index == #elements) then
			str = str.. element.value
		else
			str = str.. element.value.. ", "
		end
	end
	str = str.. ']'
	return str
end

local function struct_to_debug_str(elements)
	local done = {}
	local str = '{'
	for key, element in pairs(elements) do
		if (done[elements]) then
			str = str.. fmt("%s = %s", key, element.value)
		else
			str = str.. fmt("%s = %s", key, element.value).. ", "
			done[elements] = element
		end
	end
	str = str.. '}'
	return str
end

function eval_value(val)
	if (is_literal(val)) then return val end
	
	if (is_expr(val)) then
		return eval_expr(val)
	elseif (val.type == PARSE_TYPES.TERN_EXPR) then
		return eval_tern_expr(val.cond, val.true_val, val.false_val)
	elseif (val.type == TK_TYPES.ID) then
		-- print_pretty_tb(loop_stack)

		local id_data = search_val_from_envs("vars", val.value)
		assert(id_data,
		  fmt("Failed to retrieve identifier '%s' at line %d, column %d, since it's undeclared.",
		  val.value, val.src_line, val.src_column))

		local value = deep_clone_tb(id_data.value)
		value.src_line = val.src_line
		value.src_column = val.src_column
		return value
	elseif (val.type == PARSE_TYPES.FN_CALL) then
		run_fn_call(val)
		local ret_val = fn_ret_val
		fn_ret_val = {type = "keyword", value = "null"}
		return ret_val
	elseif (val.type == PARSE_TYPES.ARRAY) then
		local arr = deep_clone_tb(val)
		for index, element in ipairs(arr.elements) do
			arr.elements[index] = eval_value(element)
		end
		-- for log debugging
		arr.value = arr_to_debug_str(arr.elements)
		return arr
	elseif (val.type == PARSE_TYPES.STRUCT) then
		local struct = deep_clone_tb(val)
		for index, element in pairs(struct.elements) do
			struct.elements[index] = eval_value(element)
		end
		-- for log debugging
		struct.value = struct_to_debug_str(struct.elements)
		return struct
	end
end

function search_val_from_envs(type, id)
--	print_pretty_tb(loop_stack)
	for i = #loop_stack, 1, -1 do
--		print(id, i)
		local loop_frame = loop_stack[i]

		for ii = #loop_frame, 1, -1 do
			local loop_env = loop_frame[ii]
	--		print(loop_env, pretty_table(loop_frame), #loop_frame)
			if (loop_env) then
				local val = loop_env[type][id]
--				print("LOOP ENV")
--				print(val, pretty_table(loop_env.vars), pretty_table(loop_frame), "INDE", i)
				if (val) then return val end
			end
		end
	end
	for i = #call_stack, 1, -1 do
		local frame = call_stack[i]
		local val = frame.env[type][id]
		if (val) then return val end
	end

	local val = env[type][id]
	if (val) then return val end
end

function get_current_env()
	local loop_frame = loop_stack[#loop_stack]
	if (loop_frame) then
		local loop_env = loop_frame[#loop_frame]
		if (loop_env) then return loop_env end
	end
	local frame = call_stack[#call_stack]
	if (frame) then
		return frame.env
	end
	return env
end

-- real deal
local function run_while(parse_tree)
	local env = {vars = {}, fns = {}}
	local loop_frame = loop_stack[#loop_stack]
	loop_frame[#loop_frame + 1] = env
	while (cond_run_block(parse_tree.cond, parse_tree.block) and not is_fn_returning) do
		if (is_loop_breaking) then
			is_loop_breaking = false
			break
		end
		env.vars = {}
		env.fns = {}
	end
	loop_frame[#loop_frame] = nil
end

local function run_rep(parse_tree)
	local env = {vars = {}, fns = {}}
	local loop_frame = loop_stack[#loop_stack]
	loop_frame[#loop_frame + 1] = env
	repeat
		if (is_loop_breaking) then
			is_loop_breaking = false
			break
		end

		run_block(parse_tree.block)
		env.vars = {}
		env.fns = {}
	until (not is_falsy(eval_value(parse_tree.cond)))
	loop_frame[#loop_frame] = nil
end

local function run_for(parse_tree)
	local env = {vars = {}, fns = {}}
	local loop_frame = loop_stack[#loop_stack]
	loop_frame[#loop_frame + 1] = env
	
	local index_id_tk = deep_clone_tb(parse_tree.index_id_tk)
	local start = eval_value(parse_tree.start)
	local limit = eval_value(parse_tree.limit)
	local increment = eval_value(parse_tree.increment)

	assert(start.type == TK_TYPES.NUM,
	  fmt("Cannot have %s '%s' at line %d, column %d as for loop start value, only a number.",
	  start.type, start.value, start.src_line, start.src_column))
	  
	assert(limit.type == TK_TYPES.NUM,
	  fmt("Cannot have %s '%s' at line %d, column %d as for loop limit value, only a number.",
	  limit.type, limit.value, limit.src_line, limit.src_column))
	  
	assert(increment.type == TK_TYPES.NUM,
	  fmt("Cannot have %s '%s' at line %d, column %d as for loop increment value, only a number.",
	  increment.type, increment.value, increment.src_line, increment.src_column))
	  
	for index = start.value, limit.value, increment.value do
		if (is_loop_breaking) then
			is_loop_breaking = false
			break
		end
		local new_val = deep_clone_tb(start)
		new_val.value = index
		local new_index_val = {type = TK_TYPES.NUM, value = new_val,
         	  src_line = index_id_tk.src_line, src_column = index_id_tk.src_column}
		env.vars = {[index_id_tk.value] = new_index_val}
		env.fns = {}
		
		run_block(parse_tree.block)
	end
	loop_frame[#loop_frame] = nil
end

local function run_iter(parse_tree)
	local env = {vars = {}, fns = {}}
	local loop_frame = loop_stack[#loop_stack]
	loop_frame[#loop_frame + 1] = env
	-- print_parse_tree(parse_tree)

	loop_frame[#loop_frame] = nil
end

local function run_declare(parse_tree)
	local id = parse_tree.id_name
	local existing_var = get_current_env().vars[id]
	--print_pretty_tb(loop_stack)
	-- can't use assert, else it will error about nil indexing 'existing_var' for format
	if (existing_var) then
		error(fmt("Failed to declare var '%s' at line %d, column %d since it's already declared at line %d, column %d.",
		  id, parse_tree.src_line, parse_tree.src_column, existing_var.src_line, existing_var.src_column))
	end

	local value = eval_value(parse_tree.value)
	get_current_env().vars[id] = {src_line = parse_tree.src_line,
	  src_column = parse_tree.src_column, value = value}
end

local function run_reassign(parse_tree)
	local id = parse_tree.id_name
	local existing_var = search_val_from_envs("vars", id)
	
	assert(existing_var, fmt("Failed to reassign var '%s' at line %d, column %d since it's undeclared.",
	  id, parse_tree.src_line, parse_tree.src_column))

	existing_var.value = eval_value(parse_tree.value)
end

local function run_fn(parse_tree)
	local id = parse_tree.id_name
	local existing_fn = search_val_from_envs("fns", id)

	if (existing_fn) then
		if (existing_fn.is_built_in) then
			error(fmt("Failed to declare function '%s' at line %d, column %d since it's a built in global.",
			  id, parse_tree.src_line, parse_tree.src_column, existing_fn.src_line, existing_fn.src_column))
		else
			error(fmt("Failed to declare function '%s' at line %d, column %d since it's already declared at line %d, column %d.",
			  id, parse_tree.src_line, parse_tree.src_column, existing_fn.src_line, existing_fn.src_column))
		end
	end
	
	get_current_env().fns[id] = {src_line = parse_tree.src_line, src_column = parse_tree.src_column,
	  has_variadic_params = parse_tree.has_variadic_params, params = parse_tree.params, block = parse_tree.block}
end

function run_fn_call(parse_tree)
	local id = parse_tree.id_name
	local fn = search_val_from_envs("fns", id)
	assert(fn,
	 fmt("Failed to call fn '%s' at line %d, column %d since it's undeclared.",
	 id, parse_tree.src_line, parse_tree.src_column))

	-- deep clone, to prevent the parse tree's arg table itself from being modified since tables are passed by reference
	local args = deep_clone_tb(parse_tree.args)
	
	if (fn.is_built_in) then
		for index, arg in ipairs(args) do
			args[index] = eval_value(arg)
		end
		fn.run(table.unpack(args))
		return
	end

	local env = {vars = {}, fns = {}}

	for index, param in ipairs(fn.params) do
		local arg = parse_tree.args[index]
		arg = (arg) and eval_value(arg) or {type = TK_TYPES.KEYWORD, value = "null",
		  src_line = param_src_line, src_column = param.src_column}
		env.vars[param.value] = {value = arg, src_line = param.src_line, src_column = param.src_column}
	end

	call_stack[#call_stack + 1] = {id = id,
	  env = env, src_line = fn.src_line, src_column = fn.src_column, nested_block_count = 0}

	-- allow for nested loop environments inside functions
	loop_stack[#loop_stack + 1] = {}
	run_block(fn.block)
	call_stack[#call_stack] = nil
	loop_stack[#loop_stack] = nil
end

local function run_ret(parse_tree)
	fn_ret_val = eval_value(parse_tree.value)
	-- print("RET VAL", pretty_table(fn_ret_val))
	is_fn_returning = true
end

local function run_if(parse_tree)
	cond_run_block(parse_tree.cond, parse_tree.block)
	
	for _, statement in ipairs(parse_tree.elif_statements) do
		local ran = cond_run_block(statement.cond, statement.block)
		if (ran) then return end
	end
	if (parse_tree.else_block) then run_block(parse_tree.else_block) end
end

local function index_arr(arr, index_data)
	assert(arr.type == PARSE_TYPES.ARRAY,
	  fmt("Cannot index path assign %s '%s' at line %d, column %d, only arrays or structs.",
	  arr.type, arr.value, index_data.src_line, index_data.src_column))
	local index_value = index_data.value

	assert(index_data.type == PARSE_TYPES.ARRAY_INDEX,
	  fmt("Cannot have struct index '%s' for array '%s' at line %d, column %d, only number indexes.",
	  index_value.value, arr.value, index_data.src_line, index_data.src_column))
	  
	local index = eval_value(index_value)
	assert(index.type == TK_TYPES.NUM,
	  fmt("Cannot have %s '%s' as number index for array '%s' at line %d, column %d.",
	  index.type, index.value, arr.value, index_data.src_line, index_data.src_column))

	-- zero based indexing converted to lua's 1 based
	local element = arr.elements[index.value + 1]
	print(fmt("ELEMENT AT INDEX '%d': ", index.value), pretty_table(element))

	return element
end

local function index_struct(struct, index_data)

end

local function run_index_path_assign(parse_tree)
	print_pretty_tb(parse_tree)
	local id = parse_tree.id_name
	-- either struct or an array, this is why static typing is better
	local existing_var = search_val_from_envs("vars", id)
	assert(existing_var, fmt("Failed to index path assign var '%s' at line %d, column %d since it's undeclared.",
	  id, parse_tree.src_line, parse_tree.src_column))

	-- why
	local var_value = existing_var.value
	if (var_value.type == PARSE_TYPES.ARRAY) then
		local index_data = table.remove(parse_tree.index_path, 1)
		local element = index_arr(var_value, index_data)
		print("ELEMENT")
		print_pretty_tb(element)
		for i, index_data in ipairs(parse_tree.index_path) do
			element = index_arr(element, index_data)
			print("ELEMENT")
			print_pretty_tb(element)
		end
		
	elseif (var_value.type == PARSE_TYPES.STRUCT) then
		
	else
		print(pretty_table(var_value))
		error(fmt("Cannot index path assign %s '%s' at line %d, column %d, only arrays or structs.",
		  var_value.type, var_value.value, var_value.src_line, var_value.src_column))
	end
end

local function run_break()
	is_loop_breaking = true
end

local function run_skip()
	is_loop_skipping = true
end

local tasks = {
	[PARSE_TYPES.WHILE] = run_while,
	[PARSE_TYPES.DECLARE] = run_declare,
	[PARSE_TYPES.FN] = run_fn,
	[PARSE_TYPES.FN_CALL] = run_fn_call,
	[PARSE_TYPES.RET] = run_ret,
	[PARSE_TYPES.IF] = run_if,
	[PARSE_TYPES.REASSIGN] = run_reassign,
	[PARSE_TYPES.SKIP] = run_skip,
	[PARSE_TYPES.BREAK] = run_break,
	[PARSE_TYPES.REP] = run_rep,
	[PARSE_TYPES.FOR] = run_for,
	[PARSE_TYPES.ITER] = run_iter,
	[PARSE_TYPES.INDEX_PATH_ASSIGN] = run_index_path_assign
}

local function run_statement(block)
	inc_index()
	local statement = block[get_index()]
	local task = tasks[statement.type]
	task(statement)
end

function run_block(block)
	local frame = call_stack[#call_stack]
	if (frame) then frame.nested_block_count = frame.nested_block_count + 1 end
	
	push_index()
	local end_index = #block
	while (true) do
		if (is_fn_returning) then
			local frame = call_stack[#call_stack]
			frame.nested_block_count = frame.nested_block_count - 1

			if (frame.nested_block_count == 0) then
				is_fn_returning = false
			end

			break
		end
		if (is_loop_breaking) then
			--local frame = call_stack[#call_stack]
			-- if (frame) then frame.nested_block_count = frame.nested_block_count - 1 end
			break
		end
		if (is_loop_skipping) then
			is_loop_skipping = false
			break
		end
		if (get_index() == end_index) then break end
		run_statement(block)
	end
	pop_index()
	if (frame and not is_fn_returning) then frame.nested_block_count = frame.nested_block_count - 1 end
end

return function(code_parse_tree)
	run_block(code_parse_tree)
	print("LCS: ", #loop_stack, #call_stack)
end
