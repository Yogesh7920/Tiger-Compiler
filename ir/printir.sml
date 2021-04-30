structure PrintIR =
struct
    open Tree;
    fun compile ir = print_str (expr_to_str ir ^ "\n")
    and print_str s = TextIO.output (TextIO.stdOut, s)

    and exprs_to_str []         = ""    |
        exprs_to_str [x]        = expr_to_str x |
        exprs_to_str (x::xs)    = expr_to_str x ^ ", " ^ exprs_to_str xs  

    and expr_to_str (CONST i) = "CONST " ^ Int.toString(i)  |
        expr_to_str (NAME i)  = "NAME " ^ Int.toString(i)   |
        expr_to_str (TEMP i) = "TEMP " ^ Int.toString(i)    |
        expr_to_str (BINOP (oper, e1, e2)) = "BINOP (" ^ binop_to_str oper ^ ", " ^ expr_to_str e1 ^ ", " ^ expr_to_str e2 ^ ")"  |
        expr_to_str (MEM (e)) = "MEM (" ^ expr_to_str e ^ ")"  |
        expr_to_str (CALL (e, args)) = "CALL (" ^ expr_to_str e ^ ", [" ^ exprs_to_str args  ^ "]"    |
        expr_to_str (ESEQ (s, e)) = "ESEQ (" ^ stm_to_str s ^ ", " ^ expr_to_str e ^ ")"
    
    and labels_to_str []    = ""    |
        labels_to_str [x]   = Int.toString x    |
        labels_to_str (x::xs) = Int.toString x ^ ", " ^ labels_to_str xs

    and stm_to_str (MOVE (e1, e2))    =   "MOVE (" ^ expr_to_str e1 ^ ", " ^ expr_to_str e2 ^ ")" |
        stm_to_str (EXP (e))  =   "EXP (" ^ expr_to_str e ^ ")"       |
        stm_to_str (JUMP (e, ls)) = "JUMP (" ^ expr_to_str e ^ ", " ^ "[" ^ labels_to_str ls ^ "]" ^ ")"    |
        stm_to_str (CJUMP (reloper, e1, e2, l1, l2)) = "CJUMP (" ^ expr_to_str e1 ^ ", " ^ expr_to_str e2 ^ ", " ^ Int.toString l1 ^ ", " ^ Int.toString l2 ^ ")"    |
        stm_to_str (SEQ (s1, s2)) = "SEQ (" ^ stm_to_str s1 ^ ", " ^ stm_to_str s2 ^ ")"  |
        stm_to_str (LABEL (l))    = "LABEL (" ^ Int.toString(l) ^ ")"
    
    and binop_to_str PLUS = "PLUS"      |
        binop_to_str MINUS = "MINUS"    |
        binop_to_str MUL = "MUL"        |
        binop_to_str DIV = "DIV"        |
        binop_to_str AND = "AND"        |
        binop_to_str OR = "OR"
    
    and relop_to_str EQ = "EQ"  |
        relop_to_str NE = "NE"  |
        relop_to_str LT = "LT"  |
        relop_to_str GT = "GT"  |
        relop_to_str LE = "LE"  |
        relop_to_str GE = "GE"
end