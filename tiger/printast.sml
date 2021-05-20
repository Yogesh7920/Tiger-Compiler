structure PrintAST =
struct
    open Tiger;
    fun print_ast a = print_strs (ast_to_str a)
    and print_str s = TextIO.output (TextIO.stdOut, s)
    and print_strs [] = []
        | print_strs (x::xs) = (print_str x) :: (print_strs xs)
    and ast_to_str (Expr exp) = (["Expr("] @ exp_to_str exp @ [")"] @ ["\n"])
        | ast_to_str (Decs decs) = (["Decs(["] @ decs_to_str decs @ ["])"] @ ["\n"])
    and exps_to_str [] = []
        | exps_to_str (x::xs) =   (case xs of
                                    [] => (exp_to_str x) @ (exps_to_str xs)
                                    | _ => (exp_to_str x) @ [", "] @ (exps_to_str xs)
                                )
    and exp_to_str (Array a) =    let
                                    val {Type, Len, Val} = a
                                in
                                    ["Array({Type = ",  Type, ", Len = "] @ (exp_to_str Len) @ [", Val = "] @ (exp_to_str Val) @ ["})"]
                                end
        | exp_to_str (Int i) = (["Int(", Int.toString i ,")"])
        | exp_to_str (Str s) = (["Str(\"", s ,"\")"])
        | exp_to_str (Record r) = let
                                    val {Type, Val} = r
                                    val Val_terms = records_to_str Val
                                in
                                    ["Record({Type = ",  Type, ", Val = "] @ Val_terms @ ["})"]
                                end
        | exp_to_str (New t) = ["New(",  t, ")"]
        | exp_to_str (Lval l) =  ["Lval("] @ lval_to_str l @ [")"]
        | exp_to_str (FunctionCall f) =   let
                                            val {Name, Args} = f
                                            val args_val = ["["] @  exps_to_str Args @ ["]"]
                                        in
                                            ["FunctionCall({Name = ",  Name, ", Args = "] @ args_val @ ["})"]
                                        end
        | exp_to_str (MethodCall f) =     let
                                            val {Obj, Name, Args} = f
                                            val obj_val = lval_to_str Obj
                                            val args_val = ["["] @  exps_to_str Args @ ["]"]
                                        in
                                            ["MethodCall({Obj = "] @ obj_val @ [", Name = ",  Name, ", Args = "] @ args_val @ ["})"]
                                        end
        | exp_to_str (Exps e) = ["Exps["] @  exps_to_str e @ ["]"]
        | exp_to_str (Assign a) =     let
                                            val (Obj, Exp) = a
                                            val obj_val = lval_to_str Obj
                                        in
                                            ["Assign({Obj = "] @ obj_val @ [", Exp = "] @ exp_to_str Exp @ ["})"]
                                        end
        | exp_to_str (IfCond ie) =    let
                                        val {If, Then, Else} = ie
                                        val cond_val = exp_to_str If
                                        val succ_val = exp_to_str Then
                                    in
                                        case Else of
                                        SOME t => ["IfCond({If = "] @ cond_val @ [", Then = "] @ succ_val @ [", Else = "] @ (exp_to_str t) @ ["})"]
                                        | NONE => ["IfCond({If = "] @ cond_val @ [", Then = "] @ succ_val @ [", Else = NONE"] @ ["})"]
                                    end
        | exp_to_str (While w) =  let
                                    val {Cond, Body} = w
                                    val cond_val = exp_to_str Cond
                                    val body_val = exp_to_str Body
                                in
                                    ["While({Cond = "] @ cond_val @ [", Body = "] @ body_val @ ["})"]
                                end
        | exp_to_str (For f) =    let
                                    val {Name, From, To, Body} = f
                                    val exp = exp_to_str From
                                    val exit_val = exp_to_str To
                                    val body_val = exp_to_str Body
                                in
                                    ["For({name = ",  Name, ", From = "] @ exp @ [", To = "] @ exit_val @ [", Body = "] @ body_val @ ["})"]
                                end
        | exp_to_str Break = ["Break"]
        | exp_to_str (LetExp l) =    let
                                    val {Let, In} = l
                                    val decs_val = ["["] @  decs_to_str Let @ ["]"]
                                    val body_val = ["["] @  exps_to_str In @ ["]"]
                                in
                                    ["LetExp({Let = "] @ decs_val @ [", In = "] @ body_val @ ["})"]
                                end
        | exp_to_str (Oper operation) = let
                                        val (left, oper, right) = operation
                                        val opval = (oper_to_str oper)
                                    in
                                        ["Oper("] @ (exp_to_str left) @ [", "] @ opval @ [", "] @ (exp_to_str right) @ [")"]
                                    end
        | exp_to_str (NIL) = ["NIL"]
    
    and decs_to_str [] = []
        | decs_to_str (x::xs) =   (case xs of
                                    [] => (dec_to_str x) @  (decs_to_str xs)
                                    | _ => (dec_to_str x) @ [", "] @  (decs_to_str xs)
                                )
    and dec_to_str (VarDec v) =   let
                                    val {Name, Type, Val} = v;
                                    val exp = exp_to_str Val
                                in
                                    case Type of
                                    SOME t => (["VarDec({Name = ",  Name, ", Type = ",  t, ", Val = "] @ exp @ ["})"])
                                    | NONE => (["VarDec({Name = ",  Name, ", Val = "] @ exp @ ["})"])
                                end
        | dec_to_str (ClassDec c) =   let
                                        val {Name, Extends, Fields} = c
                                        val cfs = ["["] @  classfields_to_str Fields @ ["]"]
                                    in
                                        case Extends of
                                        SOME e => (["ClassDec({name = ",  Name, ", Extends = ",  e, ", Fields = "] @ cfs @ ["})"])
                                        | NONE => (["ClassDec({name = ",  Name, ", Fields = "] @ cfs @ ["})"])
                                    end

        | dec_to_str (TypeDec t) =    let
                                        val {Name, Type} = t
                                    in
                                        ["TypeDec({Name = ",  Name, ", Type = "] @ type_to_str Type @ ["})"]
                                    end

        | dec_to_str (FunDec f) = fundectype_to_str "FunDec" f
        | dec_to_str (PrimitiveDec p) = let
                                            val {Name, ArgTypes, Type} = p
                                            val tyfields = ["["] @ tyfields_to_str ArgTypes @ ["]"]
                                        in
                                            case Type of
                                            SOME t => (["PrimitiveDec({Name = ", Name, ", ArgTypes = "] @ tyfields @ [", Type = ", t] @ ["})"])
                                            | NONE => (["PrimitiveDec({Name = ", Name, ", ArgTypes = "] @ tyfields @ ["})"])
                                        end
    
    and oper_to_str Plus = ["Plus"]      | 
        oper_to_str Minus = ["Minus"]    |
        oper_to_str Mul = ["Mul"]        | 
        oper_to_str Div = ["Div"]        | 
        oper_to_str Eq = ["Eq"]          |
        oper_to_str Neq = ["Ne"]         |
        oper_to_str Gt = ["Gt"]          | 
        oper_to_str Lt = ["Lt"]          |
        oper_to_str Gte = ["Gte"]        | 
        oper_to_str Lte = ["Lte"]        | 
        oper_to_str And = ["And"]        | 
        oper_to_str Or = ["Or"]

    and lval_to_str (Var v) = ["Var(",  v, ")"]
        | lval_to_str (Member r) = let
                                        val (Obj, Name) = r
                                        val obj_val = lval_to_str Obj
                                    in
                                        ["Member("] @ obj_val @ [",",  Name] @ [")"]
                                    end
        | lval_to_str (Ref a) =   let
                                            val (Obj, index) = a
                                            val obj_val = lval_to_str Obj
                                            val index_val = exp_to_str index
                                        in
                                            ["Ref("] @ obj_val @ [","] @ index_val @ [")"]
                                        end

    and record_to_str r = let
                                val {Key, Val} = r
                            in
                                ["{Key = ", ( Key), ", Val = "] @ exp_to_str Val @ ["}, "]
                            end

    and records_to_str [] = []
        | records_to_str (x::xs) = (record_to_str x) @ (records_to_str xs)

    and tyfield_to_str t =    let
                                val {ID, Type} = t;
                            in
                                ["{", "ID = ",  ID, ", Type = ", ( Type), "}"]
                            end

    and fundectype_to_str what f =    let
                                        val {Name, ArgTypes, Type, Val} = f
                                        val tyfields = ["["] @ tyfields_to_str ArgTypes @ ["]"]
                                        val exp = exp_to_str Val
                                    in
                                        case Type of
                                        SOME t => ([what, "({Name = ",  Name, ", ArgTypes = "] @ tyfields @ [", Type = ",  t] @ [", Val = "] @ exp @ ["})"])
                                        | NONE => ([what, "({Name = ",  Name, ", ArgTypes = "] @ tyfields @ [", Val = "] @ exp @ ["})"])
                                    end

    and type_to_str (TypeAlias t) = ["TypeAlias(",  t,")"]
        | type_to_str (RecordType r) =    let
                                            val tyfields = ["["] @ tyfields_to_str r @ ["]"]
                                        in
                                            ["RecordType("] @ tyfields @ [")"]
                                        end
        | type_to_str (ArrayType a) = ["ArrayType(",  a, ")"]
        | type_to_str (ClassType c) =     let
                                            val {Fields, Extends} = c
                                            val cfs = ["["] @  classfields_to_str Fields @ ["]"]
                                        in
                                            case Extends of
                                            SOME e => (["ClassType({Extends = ",  e, ", Fields = "] @ cfs @ ["})"])
                                            | NONE => (["ClassType({Fields = "] @ cfs @ ["})"])
                                        end

    and tyfields_to_str [] = []
        | tyfields_to_str (x::xs) =   (case xs of
                                        [] => (tyfield_to_str x) @ (tyfields_to_str xs)
                                        | _ => (tyfield_to_str x) @ [", "] @ (tyfields_to_str xs)
                                    )

    and classfields_to_str [] = []
        | classfields_to_str (x::xs) =    (case xs of
                                            [] => (classfield_to_str x) @ (classfields_to_str xs)
                                            | _ => (classfield_to_str x) @ [", "] @  (classfields_to_str xs)
                                        )
    and classfield_to_str (ClassVarDec a) =   let
                                            val {Name, Type, Val} = a
                                            val exp = exp_to_str Val
                                        in
                                            case Type of
                                            SOME t => (["ClassVarDec({Name = ",  Name, ", Type = ",  t, ", Val = "] @ exp  @ ["})"])
                                            | NONE => (["ClassVarDec({Name = ",  Name, ", Val = "] @ exp @ ["})"])
                                        end
        | classfield_to_str (ClassMethodDec m) = fundectype_to_str "ClassMethodDec" m;

end