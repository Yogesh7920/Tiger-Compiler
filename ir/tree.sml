structure Tree =
struct
    exception Error

    datatype expr   =   CONST of int                    | 
                        NAME  of int                    | 
                        TEMP  of int                    | 
                        BINOP of binop * expr * expr    |
                        MEM   of expr                   |
                        CALL  of expr * expr list       |
                        ESEQ of stm * expr

    and stm        =    MOVE  of expr * expr                        | 
                        EXP   of expr                               | 
                        JUMP  of expr * int list                    |
                        CJUMP of relop * expr * expr * int * int    |
                        SEQ of stm * stm                            |
                        LABEL of int
    
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

end
