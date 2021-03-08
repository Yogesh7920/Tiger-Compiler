structure Tiger = struct

type id = string

type typeid = id
type tyfields = {ID: id, Type: typeid} list 

datatype AST    =   Exp                 |
                    Decs of Dec list

    and Exp     =   Null                                                    |
                    Int of int                                              |
                    Str of string                                           |
                    Array of {Type: typeid, Len: Exp, Val: Exp}             |
                    Record of {Type: typeid, Val: {Key: id, Val: Exp} list} |
                    New of typeid                                           |
                    Lval of Lvalue                                                  |
                    FunctionCall of {Name: id, Args: Exp list}              |
                    MethodCall of {Obj: Lvalue, Name: id, Args: Exp list}   |
                    Oper of (Exp * BinOp * Exp)                             |
                    Assign of Lvalue * Exp                                  |
                    IfCond of {If: Exp, Then: Exp, Else: Exp option}        |
                    While of {Cond: Exp, Do: Exp}                           |
                    For of {Var: id, From: Exp, To: Exp, Do: Exp}           |
                    Break                                                   |
                    LetExp of {Let: Dec list, In: Exp}                      |
                    Exps of Exp list

    and Lvalue  =   Var of id               |
                    Member of Lvalue * id   |
                    Ref of Lvalue * Exp      

    and Dec     =   TypeDec of Ty                                                             |
                    ClassDef of {Name: id, Extends: typeid option, Fields: Classfield}        |
                    VarDec of {Name: id, Type: typeid option, Val: Exp}                       |
                    FunDec of {Name: id, ArgTypes: tyfields, Type: typeid option, Val: Exp}   |
                    PrimitiveDec of {Name: id, ArgTypes: tyfields, Type: typeid option}

and Classfield  =   Classfields of Classfield list                                                      |
                    ClassVarDec of {Name: id, Type: typeid option, Val: Exp}                            |
                    ClassMethodDec of {Name: id, ArgTypes: tyfields, Type: typeid option, Val: Exp}

    and Ty      =   TypeAlias of typeid          |
                    RecordType of tyfields list  |
                    ArrayType of typeid

    and BinOp   =   Plus | Minus | Mul | Div |
                    Eq   | Neq   | Gt  | Lt  | Gte | Lte  |
                    And  | Or 

end (* structure Tiger *)


val _ = print "Hello From Tiger AST\n";


(* 
The following is the implementation of

    let
        var table := int_array[100] of 0
    in 
        table[1]
    end 
*)

val array = Tiger.Array({
                        Type="int",
                        Len=Tiger.Int(100),
                        Val=Tiger.Int(0)
                    });
val declare = Tiger.VarDec(
                {
                    Name="int_array",
                    Type=SOME("int"),
                    Val=array
                }
            );

val eval = Tiger.Ref(Tiger.Var("int_array"), Tiger.Int(1))

val x = Tiger.LetExp(
    {
        Let=[declare],
        In=Tiger.Lval(eval)
    }
)