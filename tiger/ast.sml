structure Tiger = struct

(* datatype    ast =   Stm     | 
                    ExprList    | *)


datatype    Binop   =   Plus    |
                        Minus   |
                        Mul     |
                        Div     

    and     Expr    =   Id  of string   |
                        num of int      |
                        Op  of num * BinOp * num
    
    and     ExprList =   Empty   |
                        Some of Expr list

    and     Stm     =   Print of ExprList       |
                        Assign of Id * Expr     |
                        Compound of Stm * Stm


and classfield = VarDec ...
               | Method of { method_id   : ID
                           , method_args : typefield list
                           , method_out_type : type_id option
                           , method_body : exp
                           }

(* Suggestion to use the record syntax of SML

    https://www.cs.cornell.edu/courses/cs312/2008sp/recitations/rec02.html

*)



end (* structure Tiger *)
