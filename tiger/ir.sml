signature Tree =
sig

    datatype binop = PLUS | MINUS | MUL | DIV | And | Or  
    datatype relop = EQ | NE | LT | GT | LE | LT 

    datatype expr = CONST of int                    | 
                    NAME  of Temp.label             | 
                    TEMP  of Temp.temp              | 
                    BINOP of binop * expr * expr    |
                    MEM   of expr                   |
                    CALL  of expr * expr list       |
                    ESEQ of stmt * expr

     and stmt = MOVE  of expr * expr                                    | 
                EXP   of expr                                           | 
                JUMP  of expr * Temp.label list                         |
                CJUMP of relop * expr * expr * Temp.label * Temp.label  |
                SEQ of stmt * stmt                                      |
                LABEL of Temp.label


end

signature TEMP =
sig
    type label
    type temp
    val newlabel : unit -> label  (* generate a new label *)
    val newtemp  : unit -> temp
end

structure Temp :> TEMP = struct


    type label = int (* 2⁶⁴ = ∞ many variables *)
    type temp  = int


    val nextLabel = ref 0
    val nextTemp  = ref 0

    fun newlabel _ = (* stuff here *)
    fun newtemp  _ = (* stuff here *)


end