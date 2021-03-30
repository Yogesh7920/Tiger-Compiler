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
    val level = ref 0
    fun indent (n) = 
        let 
            fun max (a, b) = if (a>b) then a else b
        in
            level := !(level) + n; level := max(!level, 0)
        end
    fun level_inc() = (indent(1); "")
    fun level_dec() = (indent(~1); "")
    fun make_indent() = tab(!level)

    fun compile a = print_strs (ast_to_str a)
    and print_str s = TextIO.output (TextIO.stdOut, s)
    and print_strs [] = []
        | print_strs (x::xs) = (print_str x) :: (print_strs xs)

    and ast_to_str (Expr exp) = (exp_to_str exp @ ["\n"])
        | ast_to_str (Decs decs) = (decs_to_str decs @ ["\n"])
    and exps_to_str [] = []
        | exps_to_str (x::xs) =   (case xs of
                                    [] => (exp_to_str x) @ (exps_to_str xs) @ ["\n"]
                                    | _ => (exp_to_str x) @ ["\n"] @ (exps_to_str xs)
                                )
    and args_to_str [] = []
        | args_to_str (x::xs) =   (case xs of
                                    [] => (exp_to_str x) @ (args_to_str xs)
                                    | _ => (exp_to_str x) @ [", "] @ (args_to_str xs)
                                )

    and exp_to_str (Array a) =    let
                                    val {Type, Len, Val} = a
                                in
                                    [type_ Type, "["] @ (exp_to_str Len) @ ["] " ^ (keyword "of ")] @ (exp_to_str Val)
                                end
        | exp_to_str (Int i) = ([digit (Int.toString i)])
        | exp_to_str (Str s) = ([string ("\"" ^ s ^ "\"")])
        | exp_to_str (Record r) = let
                                    val {Type, Val} = r
                                    val Val_terms = records_to_str Val
                                in
                                    [type_ Type, " {"] @ Val_terms @ ["}"]
                                end
        | exp_to_str (New t) = [keyword "new ",  t]
        | exp_to_str (Lval l) =  lval_to_str l
        | exp_to_str (FunctionCall f) =   let
                                            val {Name, Args} = f
                                            val args_val = args_to_str Args
                                        in
                                            [variable Name, "("] @ args_val @ [")\n"]
                                        end
        | exp_to_str (MethodCall f) =     let
                                            val {Obj, Name, Args} = f
                                            val obj_val = lval_to_str Obj
                                            val args_val = args_to_str Args
                                        in
                                            obj_val @ [".", variable Name, "("] @ args_val @ [")\n"]
                                        end
        | exp_to_str (Exps e) = exps_to_str e
        | exp_to_str (Assign a) =     let
                                            val (Obj, Exp) = a
                                            val obj_val = lval_to_str Obj
                                        in
                                            obj_val @ [" := "] @ exp_to_str Exp
                                        end
        | exp_to_str (IfCond ie) =    let
                                        val {If, Then, Else} = ie

                                        val cond_val = exp_to_str If
                                        val succ_val = exp_to_str Then

                                    in
                                        case Else of
                                        SOME t => [(keyword "if") ^ " ("] @ cond_val @ [")\n" ^ keyword "then "] @ succ_val @ ["\n" ^ (keyword "else ")] @ (exp_to_str t)
                                        | NONE => [(keyword "if") ^ " ("] @ cond_val @ [")\n" ^ keyword "then "] @ succ_val
                                    end
        | exp_to_str (While w) =  let
                                    val {Cond, Body} = w
                                    val cond_val = exp_to_str Cond
                                    val body_val = exp_to_str Body
                                in
                                    [(keyword "while") ^ " ("] @ cond_val @ [") " ^ (keyword "do")] @ body_val @ ["\n"]
                                end
        | exp_to_str (For f) =    let
                                    val {Name, From, To, Body} = f
                                    val exp = exp_to_str From
                                    val exit_val = exp_to_str To
                                    val body_val = exp_to_str Body
                                in
                                    [(keyword "for") ^ "(", variable Name, ":= "] @ exp @ [keyword " to "] @ exit_val @ [keyword "\n" ^ "do "] @ body_val
                                end
        | exp_to_str Break = ["break\n"]
        | exp_to_str (LetExp l) =   let
                                        val {Let, In} = l
                                        val decs_val = decs_to_str Let
                                        val body_val = exps_to_str In
                                    in
                                        [keyword "let", "\n"] @ decs_val @ [(keyword "in"), "\n"] @ body_val @ [keyword "end", "\n"]
                                    end
        | exp_to_str (Oper operation) = let
                                        val (left, oper, right) = operation
                                        val opval = (oper_to_str oper)
                                    in
                                        (exp_to_str left) @ opval @ (exp_to_str right)
                                    end
        | exp_to_str (NIL) = [digit "nil"]
    
    and decs_to_str [] = []
        | decs_to_str (x::xs) =   (case xs of
                                    [] => (dec_to_str x) @  (decs_to_str xs)
                                    | _ => (dec_to_str x) @ ["\n"] @ (decs_to_str xs)
                                )
    and dec_to_str (VarDec v) =   let
                                    val {Name, Type, Val} = v;
                                    val exp = exp_to_str Val
                                in
                                    case Type of
                                    SOME t => ([keyword "var ", variable Name, ": ", type_ t, " := "] @ exp @ ["\n"])
                                    | NONE => ([keyword "var ", variable Name, " := "] @ exp @ ["\n"])
                                end
        | dec_to_str (ClassDec c) =   let
                                        val {Name, Extends, Fields} = c
                                        val cfs = ["["] @  classfields_to_str Fields @ ["]"]
                                    in
                                        case Extends of
                                        SOME e => (["class ", variable Name, ", extends ",  e, ", {"] @ cfs @ ["}"])
                                        | NONE => (["class ", variable Name, ", {"] @ cfs @ ["}"])
                                    end

        | dec_to_str (TypeDec t) =    let
                                        val {Name, Type} = t
                                    in
                                        [keyword "type ", variable Name, " = "] @ type_to_str Type
                                    end

        | dec_to_str (FunDec f) = fundectype_to_str (keyword "function") f
        | dec_to_str (PrimitiveDec p) = let
                                            val {Name, ArgTypes, Type} = p
                                            val tyfields = tyfields_to_str ArgTypes
                                        in
                                            case Type of
                                            SOME t => ([variable Name, "("] @ tyfields @ ["): ", t])
                                            | NONE => ([variable Name, "("] @ tyfields @ [")"])
                                        end
    
    and oper_to_str Plus = ["+"]        | 
        oper_to_str Minus = ["-"]       |
        oper_to_str Mul = ["*"]         | 
        oper_to_str Div = ["/"]         | 
        oper_to_str Eq = ["="]          |
        oper_to_str Neq = ["<>"]        |
        oper_to_str Gt = [">"]          | 
        oper_to_str Lt = ["<"]          |
        oper_to_str Gte = [">="]        | 
        oper_to_str Lte = ["<="]        | 
        oper_to_str And = ["&"]         | 
        oper_to_str Or = ["|"]

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
                                            val index_val = exp_to_str index
                                        in
                                            obj_val @ ["["] @ index_val @ ["]"]
                                        end

    and record_to_str r = let
                                val {Key, Val} = r
                            in
                                [variable Key, " = "] @ exp_to_str Val
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

    and fundectype_to_str what f =  let
                                        val {Name, ArgTypes, Type, Val} = f
                                        val tyfields = tyfields_to_str ArgTypes

                                        val exp = exp_to_str Val
                                    in
                                        case Type of
                                        SOME t => ([what, " ", variable Name, "("] @ tyfields @ ["): ", type_ t] @ [" =\n"] @ exp)
                                        | NONE => ([what, " ", variable Name, "("] @ tyfields @ [") =\n"] @ exp)
                                    end

    and type_to_str (TypeAlias t) = [t]
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
                                            val exp = exp_to_str Val
                                        in
                                            case Type of
                                            SOME t => (["ClassVarDec({Name = ", variable Name, ", Type = ", type_ t, ", Val = "] @ exp  @ ["})"])
                                            | NONE => (["ClassVarDec({Name = ", variable Name, ", Val = "] @ exp @ ["})"])
                                        end
        | classfield_to_str (ClassMethodDec m) = fundectype_to_str "ClassMethodDec" m;

end