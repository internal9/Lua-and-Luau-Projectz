-- TODO: fix unexpected right paranthesis

--b = "((2-2)-(2-2)) * ((2-2)-(2-2)) + ((2)+(29*2))"
local str = "2323232323^"
h = [[
	↓ - ↓
]]
--warn(h)
--	Reverse PEMDAS (and no parenthesis) due to top-down parsing
local operator_levels = {
	["+"] = 1,
	["-"] = 1,
	["*"] = 2,
	["/"] = 2,
	["^"] = 3,
}

local token_formers = {
	["number"] = {
		type = "number",
		value = nil,
	},
	["+"] = {
		type = "operator",
		value = "+"
	},
	["-"] = {
		type = "operator",
		value = "-"
	},
	["*"] = {
		type = "operator",
		value = "*"
	},
	["/"] = {
		type = "operator",
		value = "/"
	},
	["^"] = {
		type = "operator",
		value = "^"
	},
	["("] = {
		type = "left_paranthesis",
		value = "("
	},
	[")"] = {
		type = "right_paranthesis",
		value = ")"
	},
}

local operator_tokens_funcs = {
	["+"] = function(operand_1, operand_2)
		return operand_1 + operand_2
	end,
	["-"] = function(operand_1, operand_2)
		return operand_1 - operand_2
	end,
	["*"] = function(operand_1, operand_2)
		return operand_1 * operand_2
	end,
	["/"] = function(operand_1, operand_2)
		return operand_1 / operand_2
	end,
	["^"] = function(operand_1, operand_2)
		return operand_1 ^ operand_2
	end,
}

local token_expecters = {
	["number"] = {
		"operator",
		"left_paranthesis"
	},
	["number_in_paranthesis"] = {
		"operator",
		"left_paranthesis",
		"right_paranthesis"
	},
	["operator"] = {
		"number",
		"left_paranthesis"
	},
	["left_paranthesis"] = {
		"number",
		"left_paranthesis",
	},
	["right_paranthesis"] = {
		"number",
		"operator",
		"right_paranthesis"
	}
}

local function DebugPrintParseTree(parse_tree)
	
end

local function TokenizeExpression(expression_str: string)
	local tokens = {}
	local incomplete_number_token = nil
	local number_token_is_decimal = false
	local incomplete_func_str = nil
	
	for i = 1, #expression_str do
		local char = expression_str:sub(i, i)
		
		--if (tonumber(char) ~= nil) then
		--	if incomplete_number_token == nil then
		--		incomplete_number_token = table.clone(token_formers.number)
		--		incomplete_number_token.value = char
		--		table.insert(tokens, incomplete_number_token)
		--	else
		--		incomplete_number_token.value ..= char
		--	end
		--elseif char == "." then
		--	if incomplete_number_token ~= nil then
		--		assert(number_token_is_decimal == false, "No two periods in number")
		--		number_token_is_decimal = true
		--		incomplete_number_token.value ..= char
		--	end
		--elseif char:match("%a") then
		--	if incomplete_func_str == nil then
		--		incomplete_func_str = char
		--	else
		--		incomplete_func_str ..= char
		--	end
		--else
		--	if (incomplete_number_token ~= nil) then
		--		local number_str = incomplete_number_token.value
		--		local last_char_index = number_str:len()
				
		--		assert(number_str:sub(last_char_index, last_char_index) ~= ".", "Number has period but no floating points")
		--		incomplete_number_token = nil
		--		number_token_is_decimal = false
		--	end
		--	if incomplete_func_str ~= nil then
		--		
		--	end
			
		--	local token_former = token_formers[char]
		--	assert(token_former ~= nil, string.format("Unexpected symbol: %s", char))
		--	local token = table.clone(token_former)
		--	table.insert(tokens, token)
		--end
	end
	
	return tokens
end

local function ScanTokensSyntax(expr_tokens)
	local paranthesis_level = 0
	local token_expecter = nil

	local first_token = expr_tokens[1]
	if first_token.type == "left_paranthesis"then
		paranthesis_level += 1 
		token_expecter = token_expecters.left_paranthesis
	else
		assert(first_token.type == "number", "Expected first a number or left paranthesis")
		token_expecter = token_expecters.number
	end
	
	for index = 2, #expr_tokens do
		local token = expr_tokens[index]
		--warn(token.type, token_expecter)
		assert(table.find(token_expecter, token.type) ~= nil, "Got: ".. token.value)
		
		if token.type == "left_paranthesis" then
			paranthesis_level += 1
		elseif token.type == "right_paranthesis" then
			paranthesis_level -= 1
		end
		
		if token.type == "number" and paranthesis_level > 0 then
			token_expecter = token_expecters.number_in_paranthesis
			continue
		end
		
		token_expecter = token_expecters[token.type]
		--print(token.type)
	end
	
	assert(paranthesis_level == 0, "Expected right paranthesis to close")
end

--TODO: implement error checking per node, rather than just one big initial iteration
local function ParseTokens(expr_tokens)
	ScanTokensSyntax(expr_tokens)
	
	local function SplitTokensFromIndex(tokens, split_index)
		local tokens_part_1 = {}
		local tokens_part_2 = {}
		
		for index = 1, split_index - 1 do
			table.insert(tokens_part_1, tokens[index])
		end
		for index = split_index + 1, #tokens do
			table.insert(tokens_part_2, tokens[index])
		end
		
		return tokens_part_1, tokens_part_2
	end
	
	local function SplitTokensIntoNodes(tokens)
		local paranthesis_level = 0
		local lowest_operator = nil
		local lowest_operator_index = nil
		local lowest_operator_level = nil
		-- Each operand being either a number token or another node is the goal
		
		while true do
			for index = #tokens, 1, -1 do
				local token = tokens[index]
				
				if token.type == "right_paranthesis" then
					paranthesis_level += 1
					continue
				end
				if token.type ==  "left_paranthesis" then
					paranthesis_level -= 1
					continue
				end
				
				if paranthesis_level ~= 0 then
					continue
				end
				if token.type ~= "operator" then
					continue
				end
				
				if lowest_operator == nil then
					lowest_operator = token
					lowest_operator_level = operator_levels[token.value]
					lowest_operator_index = index
				else
					local operator_level = operator_levels[token.value]
					
					if operator_level >= lowest_operator_level then
						continue
					end
					
					lowest_operator = token
					lowest_operator_level = operator_level
					lowest_operator_index = index
				end
			end
			--assert(paranthesis_level == 0, "Unexpected extra right paranthesis")
			if lowest_operator ~= nil then
				break
			elseif #tokens == 1 then
				return tokens[1]
			else
				table.remove(tokens, #tokens)
				table.remove(tokens, 1)
				warn(tokens)
			end
		end
		
		if lowest_operator == nil then
			return tokens
		end

		local tokens_part_1, tokens_part_2 = SplitTokensFromIndex(tokens, lowest_operator_index)

		--assert(#tokens_part_1 ~= 0, "Expected 1st number for: ".. lowest_operator.value)
		--assert(#tokens_part_2 ~= 0, "Expected 2nd number for: ".. lowest_operator.value)

		local operand_1, operand_2 = nil

		if #tokens_part_1 == 1 then
			local token = tokens_part_1[1]
			--assert(token.type == "number", "Expected number, got: ".. token.value)
			operand_1 = token
		else
			operand_1 = SplitTokensIntoNodes(tokens_part_1)
		end

		if #tokens_part_2 == 1 then
			local token = tokens_part_2[1]
			--assert(token.type == "number", "Expected number, got: ".. token.value)
			operand_2 = token
		else
			operand_2 = SplitTokensIntoNodes(tokens_part_2)
		end
		
		local node = {
			operator = lowest_operator.value,
			operand_1 = operand_1,
			operand_2 = operand_2,
			is_node = true,
		}
		
		return node
	end
	
	local TopNode = SplitTokensIntoNodes(expr_tokens)
	print(TopNode)
	
	return TopNode
end

--NOTE: i also call the parse_tree the top node
local function EvaulateParseTree(parse_tree)
	local function evaulate_node(node)
		local result = 0
		local operand_1 = node.operand_1.is_node and evaulate_node(node.operand_1) or tonumber(node.operand_1.value)
		local operand_2 = node.operand_2.is_node and evaulate_node(node.operand_2) or tonumber(node.operand_2.value)
		
		return operator_tokens_funcs[node.operator](operand_1, operand_2)
	end
	
	return evaulate_node(parse_tree)
end

local tokens = TokenizeExpression(str)
--local parse_tree = ParseTokens(tokens)
--local result = EvaulateParseTree(parse_tree)

--print(str.. ": ".. result)
