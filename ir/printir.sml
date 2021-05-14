(* structure Printtree =
struct
    open Tree;
    fun printtree ir = print_str (expr_to_str ir ^ "\n")
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
end *)


structure Printtree : 
     sig val printtree : TextIO.outstream * Tree.stm -> unit end =

struct

structure T = Tree
    fun printtree (outstream, s0) =
    let 
        fun say s =  TextIO.output(outstream,s)
        fun sayln s= (say s; say "\n") 

        fun specialTemp x = 
                case x of
                29 => "sp"  | 
                30 => "fp"  |
                31 => "ret" |
                num => "t" ^ Int.toString num

        fun indent 0 = ()
            | indent i = (say " "; indent(i-1))

        fun stm(T.SEQ(a,b),d) =
                (indent d; sayln "SEQ("; stm(a,d+1); sayln ","; stm(b,d+1); say ")")
            | stm(T.LABEL lab, d) = (indent d; say "LABEL "; say (Int.toString lab))
            | stm(T.JUMP (e,_), d) =  (indent d; sayln "JUMP("; exp(e,d+1); say ")")
            | stm(T.CJUMP(r,a,b,t,f),d) = (indent d; say "CJUMP(";
                        relop r; sayln ",";
                        exp(a,d+1); sayln ","; exp(b,d+1); sayln ",";
                        indent(d+1); say(Int.toString t); 
                        say ","; say (Int.toString f); say ")")
            | stm(T.MOVE(a,b),d) = (indent d; sayln "MOVE("; exp(a,d+1); sayln ",";
                        exp(b,d+1); say ")")
            | stm(T.EXP e, d) = (indent d; sayln "EXP("; exp(e,d+1); say ")")

        and exp(T.BINOP(p,a,b),d) = (indent d; say "BINOP("; binop p; sayln ",";
                        exp(a,d+1); sayln ","; exp(b,d+1); say ")")
            | exp(T.MEM(e),d) = (indent d; sayln "MEM("; exp(e,d+1); say ")")
            | exp(T.TEMP t, d) = (indent d; say ("TEMP " ^ specialTemp t))

            | exp(T.ESEQ(s,e),d) = (indent d; sayln "ESEQ("; stm(s,d+1); sayln ",";
                    exp(e,d+1); say ")")
            | exp(T.NAME lab, d) = (indent d; say "NAME "; say (Int.toString lab))
            | exp(T.CONST i, d) = (indent d; say "CONST "; say(Int.toString i))
            | exp(T.CALL(e,el),d) = (indent d; sayln "CALL("; exp(e,d+1);
                    app (fn a => (sayln ","; exp(a,d+2))) el;
                    say ")")

        and binop T.PLUS = say "PLUS"
            | binop T.MINUS = say "MINUS"
            | binop T.MUL = say "MUL"
            | binop T.DIV = say "DIV"
            | binop T.AND = say "AND"
            | binop T.OR = say "OR"

        and relop T.EQ = say "EQ"
            | relop T.NE = say "NE"
            | relop T.LT = say "LT"
            | relop T.GT = say "GT"
            | relop T.LE = say "LE"
            | relop T.GE = say "GE"
    in  
        stm(s0,0); sayln ""; TextIO.flushOut outstream
    end
end