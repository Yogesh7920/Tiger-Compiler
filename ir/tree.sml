structure Tree =
struct

    datatype expr   =   CONST of int                    | 
                        NAME  of Temp.label             | 
                        TEMP  of Temp.temp              | 
                        BINOP of binop * expr * expr    |
                        MEM   of expr                   |
                        CALL  of expr * expr list       |
                        ESEQ of stm * expr

    and stm        =    MOVE  of expr * expr                                    | 
                        EXP   of expr                                           | 
                        JUMP  of expr * Temp.label list                         |
                        CJUMP of relop * expr * expr * Temp.label * Temp.label  |
                        SEQ of stm * stm                                      |
                        LABEL of Temp.label
    
    and binop       =   PLUS | MINUS | MUL | DIV | And | Or 
    and relop       =   EQ | NE | LT | GT | LE | GE 

    fun notRelop    EQ = NE |
        notRelop    NE = EQ |
        notRelop    LT = GE |
        notRelop    GT = LE |
        notRelop    LE = GT |
        notRelop    GE = LT

end
