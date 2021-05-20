
structure Printtree : 
     sig val printtree : TextIO.outstream * Tree.stm -> unit end =

struct
    structure F = Frame
    structure T = Tree
    fun printtree (outstream, s0) =
    let         
        fun say s =  TextIO.output(outstream,s)
        fun sayln s= (say s; say "\n") 

        fun specialTemp x = 
                case x of
                29  => "sp" | 
                30  => "fp" |
                2   => "v0" |
                31  => "ra" |
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