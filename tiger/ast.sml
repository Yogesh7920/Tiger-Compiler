structure Tiger = struct

type id = string

type typeid = id
type tyfields = {ID: id, Type: typeid} list 

datatype AST    =   Exp                 |
                    Decs of Dec list

    and Exp     =   Null                                                    |
                    Int                                                     |
                    Str                                                     |
                    Array of {Type: typeid, Len: Exp, Val: Exp}             |
                    Record of {Type: typeid, Val: {Key: id, Val: Exp} list} |
                    New of typeid                                           |
                    Lvalue                                                  |
                    FunctionCall of {Name: id, Args: Exp list}              |
                    MethodCall of {Obj: Lvalue, Name: id, Args: Exp list}   |
                    Oper of (Exp * BinOp * Exp)                             |
                    Assign of Lvalue * Exp                                  |
                    IfCond of {If: Exp, Then: Exp, Else: Exp option}        |
                    While of {Cond: Exp, Do: Exp}                           |
                    For of {Var: id, From: Exp, To: Exp, Do: Exp}           |
                    Break                                                   |
                    LetExp of {Let: Decs, In: Exps}                         |
                    Exps of Exp list

    and Lvalue  =   Var of id       |
                    Member of id    |
                    Ref of Exp      

    and Dec     =   TypeDec of Ty                                                               |
                    ClassDef of {Name: id, Extends: typeid option, Fields: Classfield}          |
                    VarDec of {Name: id, Type: typeid option, Val: Exp}                         |
                    FunDec of {Name: id, ArgTypes: typefields, Type: typeid option, Val: Exp}   |
                    PrimitiveDec of {Name: id, ArgTypes: typefields, Type: typeid option}

    and Classfield =    Classfields of Classfield list                                                      |
                        ClassVarDec of {Name: id, Type: typeid option, Val: Exp}                            |
                        ClassMethodDec of {Name: id, ArgTypes: typefields, Type: typeid option, Val: Exp}

    and     Ty     =    TypeAlias of typeid            |
                        RecordType of typefields list  |
                        ArrayType of typeid

    and     BinOp  =    Plus | Minus | Mul | Div |
                        Eq | Neq | Gt | Lt | Gte | Lte  |
                        And | Or 

end (* structure Tiger *)
