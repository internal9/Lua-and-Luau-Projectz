  GNU nano 8.5                        sbl2.lua                                  
end

-- pratt parsing
local function null_denot(tk)
        if (tk.type == TK_TYPES.NUM) then
                return tk

        elseif (tk.value == '-') then
                return {type = "unary", op = tk.value, right = parse_expr()}

        elseif (tk.type == TK_TYPES.ID) then

        else
                error(fmt("Expected number, identifier, '-', or '(' token for p>
        end
end

function parse_expr(prec_limit)
        prec_limit = prec_limit or 0

        local left_tk = next_tk()
        assert(left_tk, "Expected number, identifier, '-', or '(' token for par>

        local left = null_denot(left_tk)

        -- temp solution for condition
        while (peek_tk() and NUM_OP_PRECS[peek_tk().value] and NUM_OP_PRECS[pee>
                local op_tk = next_tk()
                local prec = NUM_OP_PRECS[op_tk.value]
         [ line 105/310 (33%), col 39/39 (100%), char 2277/7925 (28%) ]
^G Help      ^O Write Out ^F Where Is  ^K Cut       ^T Execute   ^C Location
^X Exit      ^R Read File ^\ Replace   ^U Paste     ^J Justify   ^/ Go To Line
