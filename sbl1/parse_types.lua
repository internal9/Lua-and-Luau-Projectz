local PARSE_TYPES = {
        FN = "fn",
        FN_CALL = "fn_call",
        ARRAY = "array",
        STRUCT = "struct",
        INDEX_PATH_ASSIGN = "index_path_assign",
        INDEX_PATH = "index_path",
        STRUCT_INDEX = "struct_index",
        ARRAY_INDEX = "array_index",
        REASSIGN = "reassign",
        RET = "ret",
        SKIP = "skip",
        BREAK = "break",
        WHILE = "while",
        FOR = "for",
        REP = "rep",
        ITER = "iter",
        IF = "if",
        CASES = "cases",
        ELIF = "elif",
        DECLARE = "declare",
        BLOCK = "block",
        BIN_EXPR = "binary_expr",
        UNA_EXPR = "unary_expr",
        TERN_EXPR = "ternary_expr"
}

return PARSE_TYPES
