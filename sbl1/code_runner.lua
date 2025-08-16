local pretty_table = require("pretty_table")
local function print_pretty_tb(tb) print(pretty_table(tb)) end

local fmt = string.format

local TK_TYPES = require("tk_types")
local PARSE_TYPES = require("parse_types")
local env = {}

-- built in globals
local function G_log(...)
	for _, arg in ipairs({...}) do
		print(arg.value)
	end
end

-- log global environment
local function G_log_genv()
	print_pretty_tb(env)
end

-- stuff
env.vars = {}
env.fns = {
	log = {run = G_log, is_built_in = true},
	log_genv = {run = G_log_genv, is_built_in = true}
		
}

local call_stack = {}
local loop_stack = {{}}
local index_stack = {}

local fn_is_returning = false
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

local function is_expr(val)
	return (val.type == PARSE_TYPES.UNA_EXPR or val.type == PARSE_TYPES.BIN_EXPR) or
	false
end

local function is_literal(val)
	return (val.type == TK_TYPES.STR or val.type == TK_TYPES.NUM or
	  val.value == "true" or val.value == "false" or val.value == "null") or
	  false
end

local function eval_str_expr(left, op, right)
	assert(op.value == '+',
	  fmt("Invalid %s operator '%s' at line %d, column %d, can only concatenate strings with operator '+'.",
	  op.type, op.value, op.src_line, op.src_column))

	assert(right.type == TK_TYPES.STR,
	  fmt("Cannot concatenate string with %s at line %d, column %d.",
	  right.type, op.src_line, op.src_column))
	  
	return {type = TK_TYPES.STR, value = left.value.. right.value,
	  src_line = left.src_line, src_column = left.src_column}
end

local function eval_expr(parse_tree)
	local left = eval_value(parse_tree.left)
	local right = eval_value(parse_tree.right)
	local op_tk = parse_tree.op_tk

	if (left.type == TK_TYPES.STR) then
		return eval_str_expr(left, op_tk, right)
	end

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

function eval_value(val)
	if (is_literal(val)) then return val end
	
	if (is_expr(val)) then
		return eval_expr(val)
	elseif (val.type == TK_TYPES.ID) then
		local id_data = search_val_from_envs("vars", val.value)
		assert(id_data,
		  fmt("Failed to retrieve identifier '%s' at line %d, column %d, since it is undeclared.",
		  val.value, val.src_line, val.src_column))

		--[[
		local value = deep_clone_tb(id_data.value)
		value.src_line = val.src_line
		value.src_column = val.src_column
		]]
		return id_data.value
	elseif (val.type == PARSE_TYPES.FN_CALL) then
		run_fn_call(val)
		return fn_ret_val
	end
end

function search_val_from_envs(type, id)
	local val = env[type][id]
	if (val) then return val end

	for i = #call_stack, 1, -1 do
		local frame = call_stack[i]
		local val = frame.env[type][id]
		if (val) then return val end		
	end
end

local function get_current_env()
	if (#call_stack ~= 0) then return call_stack[#call_stack].env end
	return env
end

-- real deal
local function run_while()
	
end

local function run_declare(parse_tree)
	local id = parse_tree.id_name
	local existing_var = get_current_env().vars[id]
	
	-- can't use assert, else it will error about nil indexing 'existing_var' for format
	if (existing_var) then
		error(fmt("Failed to declare var '%s' at line %d, column %d since it's already declared at line %d, column %d.",
		  id, parse_tree.src_line, parse_tree.src_column, existing_var.src_line, existing_var.src_column))
	end

	local value, type = eval_value(parse_tree.value)

	print(id, get_current_env() == env)
	get_current_env().vars[id] = {src_line = parse_tree.src_line,
	  src_column = parse_tree.src_column, value = value, type = type}
end

local function run_fn(parse_tree)
	local id = parse_tree.id_name
	local existing_fn = search_val_from_envs("fns", id)

	if (existing_fn) then
		error(fmt("Failed to declare var '%s' at line %d, column %d since it's already declared at line %d, column %d.",
		  id, parse_tree.src_line, parse_tree.src_column, existing_fn.src_line, existing_fn.src_column))
	end
	
	get_current_env().fns[id] = {src_line = parse_tree.src_line, src_column = parse_tree.src_column,
	  has_variadic_params = parse_tree.has_variadic_params, params = parse_tree.params, block = parse_tree.block}
end

function run_fn_call(parse_tree)
	local id = parse_tree.id_name
	local fn = search_val_from_envs("fns", id)
	assert(fn,
	 fmt("Failed to call fn '%s' at line %d, column %d since it is undeclared.",
	 id, parse_tree.src_line, parse_tree.src_column))

	if (fn.is_built_in) then
		fn.run()
		return
	end
	-- deep clone, to prevent the parse tree's arg table itself from being modified since tables are passed by reference
	call_stack[#call_stack + 1] = {id = id, args = deep_clone_tb(parse_tree.args),
	  env = {vars = {}, fns = {}}, src_line = fn.src_line, src_column = fn.src_column}

	run_block(fn.block)
end

local function run_ret(parse_tree)
	fn_ret_val = eval_value(parse_tree.value)
	is_fn_returning = true
	call_stack[#call_stack] = nil
end

local tasks = {
	[PARSE_TYPES.WHILE] = run_while,
	[PARSE_TYPES.DECLARE] = run_declare,
	[PARSE_TYPES.FN] = run_fn,
	[PARSE_TYPES.FN_CALL] = run_fn_call,
	[PARSE_TYPES.RET] = run_ret,
}

local function run_statement(block)
	inc_index()
	local statement = block[get_index()]
	local task = tasks[statement.type]
	task(statement)
end

function run_block(block)
	push_index()
	local end_index = #block
	while (get_index() < end_index and not is_fn_returning) do
		run_statement(block)
	end
	pop_index()
	is_fn_returning = false
end

return function(code_parse_tree)
	run_block(code_parse_tree)
end
