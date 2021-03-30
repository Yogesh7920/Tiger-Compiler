structure PP = 
struct
    open Tiger;
    
    fun keyword s = "\027[;36m" ^ s ^ "\027[0m" 
    (* cyan *)
    
    fun string s = "\027[;32m" ^ s ^ "\027[0m" 
    (* green *)

    fun digit s = "\027[;34m" ^ s ^ "\027[0m"
    (* blue *)

    fun variable s = "\027[;33m" ^ s ^ "\027[0m"
    (* yellow *)

    fun type_ s = "\027[;35m" ^ s ^ "\027[0m"
    (* magenta *)

    fun tab(n) = 
        let
            fun loop (i) = if (i=n) then "" else "\t"^loop(i+1)
        in 
            loop(0)
        end
    fun indent (n) = 
        let 
            fun max (a, b) = if (a>b) then a else b
        in
            tab (max (n, 0))
        end

    fun compile a = print_strs (ast_to_str a)
    and print_str s = TextIO.output (TextIO.stdOut, s)
    and print_strs [] = []
        | print_strs (x::xs) = (print_str x) :: (print_strs xs)

    and ast_to_str (Expr exp) = (exps_to_str [exp] 0 @ ["\n"])
        | ast_to_str (Decs decs) = (decs_to_str decs 0 @ ["\n"])

    and exps_to_str [] _ = []
        | exps_to_str (x::xs) level =   (case xs of
                                    [] => [indent(level)] @ (exp_to_str x level)
                                    | _ => [indent(level)] @ (exp_to_str x level) @ ["\n"] @ (exps_to_str xs level)
                                )
    and args_to_str [] _ = []
        | args_to_str (x::xs) level =   (case xs of
                                    [] => (exps_to_str [x] level) @ (args_to_str xs 0)
                                    | _ => (exps_to_str [x] level) @ [", "] @ (args_to_str xs 0)
                                )

    and exp_to_str (Array a) level =    let
                                    val {Type, Len, Val} = a
                                in
                                    [type_ Type, "["] @ (exps_to_str [Len] 0) @ ["] " ^ (keyword "of ")] @ (exps_to_str [Val] 0)
                                end
        | exp_to_str (Int i) level = ([digit (Int.toString i)])
        | exp_to_str (Str s) level = ([string ("\"" ^ s ^ "\"")])
        | exp_to_str (Record r) level = let
                                    val {Type, Val} = r
                                    val Val_terms = records_to_str Val
                                in
                                    [type_ Type, " {"] @ Val_terms @ ["}"]
                                end
        | exp_to_str (New t) level = [keyword "new ",  t]
        | exp_to_str (Lval l) level = lval_to_str l
        | exp_to_str (FunctionCall f) level =   let
                                            val {Name, Args} = f
                                            val args_val = args_to_str Args 0
                                        in
                                            [variable Name, "("] @ args_val @ [")"]
                                        end
        | exp_to_str (MethodCall f) level =     let
                                            val {Obj, Name, Args} = f
                                            val obj_val = lval_to_str Obj
                                            val args_val = args_to_str Args level
                                        in
                                            obj_val @ [".", variable Name, "("] @ args_val @ [")"]
                                        end
        | exp_to_str (Exps e) level = exps_to_str e level
        | exp_to_str (Assign a) level =     let
                                            val (Obj, Exp) = a
                                            val obj_val = lval_to_str Obj
                                        in
                                            obj_val @ [" := "] @ exps_to_str [Exp] 0
                                        end
        | exp_to_str (IfCond ie) level =    let
                                        val {If, Then, Else} = ie

                                        val cond_val = exps_to_str [If] 0
                                        val succ_val = exps_to_str [Then] 0

                                    in
                                        case Else of
                                        SOME t => [(keyword "if") ^ " ("] @ cond_val @ [")\n" ^ indent(level) ^ keyword "then "] @ succ_val @ ["\n" ^ indent(level) ^ (keyword "else ")] @ (exps_to_str [t] 0)
                                        | NONE => [(keyword "if") ^ " ("] @ cond_val @ [")\n" ^ indent(level) ^ keyword "then "] @ succ_val
                                    end
        | exp_to_str (While w) level =  let
                                    val {Cond, Body} = w
                                    val cond_val = exps_to_str [Cond] 0
                                    val body_val = exps_to_str [Body] 0
                                in
                                    [(keyword "while") ^ " ("] @ cond_val @ [") " ^ (keyword "do ")] @ body_val @ ["\n"]
                                end
        | exp_to_str (For f) level =    let
                                    val {Name, From, To, Body} = f
                                    val exp = exps_to_str [From] 0
                                    val exit_val = exps_to_str [To] 0
                                    val body_val = exps_to_str [Body] (level+1)
                                in
                                    [(keyword "for") ^ "(", variable Name, ":= "] @ exp @ [keyword " to "] @ exit_val @ [")" ^ keyword " do\n" ] @ body_val
                                end
        | exp_to_str Break level = ["break\n"]
        | exp_to_str (LetExp l) level =   let
                                        val {Let, In} = l
                                        val decs_val = decs_to_str Let (level+1)
                                        val body_val = exps_to_str In (level+1)
                                    in
                                        [keyword "let", "\n"] @ decs_val @ [(keyword "\nin\n")] @ body_val @ [keyword "\nend", "\n"]
                                    end
        | exp_to_str (Oper operation) level = let
                                        val (left, oper, right) = operation
                                        val opval = (oper_to_str oper)
                                    in
                                        (exps_to_str [left] 0) @ opval @ (exps_to_str [right] 0)
                                    end
        | exp_to_str (NIL) level = [digit "nil"]
    
    and decs_to_str [] _ = []
        | decs_to_str (x::xs) level =   (case xs of
                                    [] => [indent(level)] @ (dec_to_str x level) @  (decs_to_str xs level)
                                    | _ => [indent(level)] @ (dec_to_str x level) @ ["\n"] @ (decs_to_str xs level)
                                )
    and dec_to_str (VarDec v) _ =   let
                                    val {Name, Type, Val} = v;
                                    val exp = exps_to_str [Val] 0
                                in
                                    case Type of
                                    SOME t => ([keyword "var ", variable Name, ": ", type_ t, " := "] @ exp)
                                    | NONE => ([keyword "var ", variable Name, " := "] @ exp)
                                end
        | dec_to_str (ClassDec c) _ =   let
                                        val {Name, Extends, Fields} = c
                                        val cfs = ["["] @  classfields_to_str Fields @ ["]"]
                                    in
                                        case Extends of
                                        SOME e => (["class ", variable Name, ", extends ",  e, ", {"] @ cfs @ ["}"])
                                        | NONE => (["class ", variable Name, ", {"] @ cfs @ ["}"])
                                    end

        | dec_to_str (TypeDec t) _ =    let
                                        val {Name, Type} = t
                                    in
                                        [keyword "type ", type_ Name, " = "] @ type_to_str Type
                                    end

        | dec_to_str (FunDec f) level = fundectype_to_str (keyword "function") f level
        | dec_to_str (PrimitiveDec p) _ = let
                                            val {Name, ArgTypes, Type} = p
                                            val tyfields = tyfields_to_str ArgTypes
                                        in
                                            case Type of
                                            SOME t => ([variable Name, "("] @ tyfields @ ["): ", t])
                                            | NONE => ([variable Name, "("] @ tyfields @ [")"])
                                        end
    
    and oper_to_str Plus = [" + "]        | 
        oper_to_str Minus = [" - "]       |
        oper_to_str Mul = [" * "]         | 
        oper_to_str Div = [" / "]         | 
        oper_to_str Eq = [" = "]          |
        oper_to_str Neq = [" <> "]        |
        oper_to_str Gt = [" > "]          | 
        oper_to_str Lt = [" < "]          |
        oper_to_str Gte = [" >= "]        | 
        oper_to_str Lte = [" <= "]        | 
        oper_to_str And = [" & "]         | 
        oper_to_str Or = [" | "]

    and lval_to_str (Var v) = [variable v]
        | lval_to_str (Member r) = let
                                        val (Obj, Name) = r
                                        val obj_val = lval_to_str Obj
                                    in
                                        obj_val @ [".", variable Name]
                                    end
        | lval_to_str (Ref a) =   let
                                            val (Obj, index) = a
                                            val obj_val = lval_to_str Obj
                                            val index_val = exps_to_str [index] 0
                                        in
                                            obj_val @ ["["] @ index_val @ ["]"]
                                        end

    and record_to_str r = let
                                val {Key, Val} = r
                            in
                                [variable Key, " = "] @ exps_to_str [Val] 0
                            end

    and records_to_str [] = []
        | records_to_str (x::xs) = (case xs of
                                        [] => (record_to_str x) @ (records_to_str xs)
                                        | _ => (record_to_str x) @ [", "] @ (records_to_str xs)
                                    ) 

    and tyfield_to_str t =    let
                                val {ID, Type} = t;
                            in
                                [variable ID, ":", type_ Type]
                            end

    and fundectype_to_str what f level =  let
                                        val {Name, ArgTypes, Type, Val} = f
                                        val tyfields = tyfields_to_str ArgTypes

                                        val exp = exps_to_str [Val] (level+1)
                                    in
                                        case Type of
                                        SOME t => ([what, " ", variable Name, "("] @ tyfields @ ["): ", type_ t] @ [" =\n"] @ exp)
                                        | NONE => ([what, " ", variable Name, "("] @ tyfields @ [") =\n"] @ exp)
                                    end

    and type_to_str (TypeAlias t) = [type_ t]
        | type_to_str (RecordType r) =    let
                                            val tyfields = tyfields_to_str r
                                        in
                                            ["{"] @ tyfields @ ["}"]
                                        end
        | type_to_str (ArrayType a) = [keyword "array of ", type_ a]
        | type_to_str (ClassType c) =     let
                                            val {Fields, Extends} = c
                                            val cfs = ["["] @  classfields_to_str Fields @ ["]"]
                                        in
                                            case Extends of
                                            SOME e => (["class extends = ", e, ", {"] @ cfs @ ["}"])
                                            | NONE => (["class {"] @ cfs @ ["}"])
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
                                            val exp = exps_to_str [Val] 0
                                        in
                                            case Type of
                                            SOME t => (["ClassVarDec({Name = ", variable Name, ", Type = ", type_ t, ", Val = "] @ exp  @ ["})"])
                                            | NONE => (["ClassVarDec({Name = ", variable Name, ", Val = "] @ exp @ ["})"])
                                        end
        | classfield_to_str (ClassMethodDec m) = fundectype_to_str "ClassMethodDec" m 0;

end