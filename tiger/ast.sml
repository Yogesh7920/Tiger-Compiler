structure Tiger = struct

(* datatype    ast =   Stm     | 
                    ExprList    | *)


datatype    AST    =    Exp     |
                        Decs    

    and     Exp    =    Int     |
                        Str     |
                        Op of Exp * BinOp * Exp
    
    and     BinOp  =    Plus    |
                        Minus   |
                        Mul     |
                        Div 

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
