structure Tree =
struct
    exception Error

    datatype expr   =   CONST of int                    | 
                        NAME  of int  (* Temp.label *)  | 
                        TEMP  of int  (* Temp.temp *)   | 
                        BINOP of binop * expr * expr    |
                        MEM   of expr                   |
                        CALL  of expr * expr list       |
                        ESEQ of stm * expr

    and stm        =    MOVE  of expr * expr                        | 
                        EXP   of expr                               | 
                        JUMP  of expr * int list  (* Temp.label list *) |
                        CJUMP of relop * expr * expr * int * int  (* Temp.label, Temp.label *)  |
                        SEQ of stm * stm                            |
                        LABEL of int    (* Temp.label *)
    
    and binop       =   PLUS | MINUS | MUL | DIV | AND | OR 
    and relop       =   EQ | NE | LT | GT | LE | GE 

    fun notRelop    EQ = NE |
        notRelop    NE = EQ |
        notRelop    LT = GE |
        notRelop    GT = LE |
        notRelop    LE = GT |
        notRelop    GE = LT

    fun list_to_SEQ []       = raise Error      |
        list_to_SEQ [x]      = x                |
        list_to_SEQ (x::xs) = SEQ (x, list_to_SEQ xs)
    
    fun SEQ_to_list (SEQ(stm1, stm2)) = (SEQ_to_list stm1) @ (SEQ_to_list stm2) |
        SEQ_to_list stm = [stm]

end
