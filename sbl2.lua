--[=[
  gotta implement parsing
]=]

local pretty_table = require("prettytable")

local fmt = string.format
local function pretty_tb_print(tb) print(pretty_table(tb)) end

local TK_TYPES = {
	KEYWORD = "keyword",
	ID = "id",
	STR = "str",
	NUM = "num",
	OP = "op",
	COMP_OP = "comp_op",
	MISC = "misc",
}

local src_text;
local tokens = {}
local parse_tree = {}


local function expect_tk(type, value, index)
	local tk = tokens[index]
	
	if (value) then
		-- param 'type' is assumed
		assert(tk.value == value, fmt("Invalid %s token '%s', expected %s token '%s'", tk.type, tk.value, type, value))
	else
		assert(tk.type == type, fmt("Invalid %s token '%s', expected %s token", tk.type, tk.value, type))
	end
	
	return tk
end

local function parse_tokens()
	local tk_index = 1
	
	while (true) do
		local tk = tokens[tk_index]

		if (tk.value == "var") then
			expect_tk(TK_TYPES.MISC, '=', tk_index + 1)
			local value_tk = tokens[tk_index + 2]

			if (value_tk.type == TK_TYPES.NUM) then

			elseif (value_tk.type == TK_TYPES.STR) then
			
			elseif (value_tk.type == TK_TYPES.ID) then

			elseif (value_tk.value == '(') then

			else
				error(fmt("Invalid %s token '%s', starting at index %d. Expected a num, str, id, or '(' token",
				 value_tk.type, value_tk.value, value_tk.src_start_index))
			end
			
		elseif (tk.value == "fn") then
			
		elseif (tk.value == "if") then
			
		else
			error(fmt("Invalid %s token '%s', starting at index %d", tk.type, tk.value, tk.src_start_index))
		end
	end
end

local function lex_src_text()
	local char_index = 1
	local current_str = ""

	-- i hate it here
	local function get_op_or_comp_op(op)
		local char = string.sub(src_text, char_index + 1, char_index + 1)

		if (char == '=') then
			char_index = char_index + 1
			return TK_TYPES.COMP_OP, op.. '='
		else
			-- regular '+' op
			return TK_TYPES.OP, op
		end
	end
	
	local CHARS_TO_TKS = {
		['+'] = get_op_or_comp_op,
		['-'] = get_op_or_comp_op,

		['='] = TK_TYPES.MISC,
		['!'] = TK_TYPES.MISC,
		['('] = TK_TYPES.MISC,
		[')'] = TK_TYPES.MISC,
	}

	local KEYWORDS = {
		["var"] = true,
		["fn"] = true,
		["if"] = true,
		["else"] = true,
		["eif"] = true,
		["end"] = true,
		[""] = true,
		["if"] = true,
		["if"] = true,

	}


	while (true) do
		local char = string.sub(src_text, char_index, char_index)
		
		if (string.match(char, '%s')) then
			char_index = char_index + 1

		elseif (CHARS_TO_TKS[char]) then
			local type_or_handler = CHARS_TO_TKS[char]
			local token_type, value;
			local src_text_start_index
			
			if (type(type_or_handler) == "function") then 
				token_type, value = type_or_handler(char)
				src_text_start_index = char_index - 1
			else
				token_type = type_or_handler
				value = char
				src_text_start_index = char_index
			end
			
			table.insert(tokens, { type = token_type, value = value, src_text_start_index = src_text_start_index })
			char_index = char_index + 1
			
		elseif (string.match(char, '%d')) then
			local NUM_START_INDEX = char_index
		
			while (true) do
				char_index = char_index + 1
				char = string.sub(src_text, char_index, char_index)
				
				if (not string.match(char, '%d')) then
					local num = string.sub(NUM_START_INDEX, char_index - 1)
					table.insert(tokens, { type = TK_TYPES.NUM, value = value, src_start_index = NUM_START_INDEX })
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

		else
			-- do some error lol
		end

--		print(char_index == #src_text, char_index, #src_text)

		if (char_index == #src_text) then
			break
		end
	end
end

local function main()
        local file_name = arg[1]
        assert(file_name ~= nil, "SBL: file name expected")
        assert(string.sub(file_name, #file_name - 3, #file_name) == ".sbl", "SBL: file '".. file_name.. "' must have 'sbl' file extension")

        local file = io.open(file_name, 'r')
        assert(file ~= nil, "SBL: '".. file_name.. "' is not a valid file")

        src_text = file:read("*all")
        
        lex_src_text()
        print(pretty_tb_print(tokens))

	parse_tokens(tokens)
end

main()
