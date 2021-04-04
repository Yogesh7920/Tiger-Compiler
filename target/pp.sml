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

    fun func s = "\027[;31m" ^ s ^ "\027[0m"
    (* red  *)

    fun type_ s = "\027[;35m" ^ s ^ "\027[0m"
    (* magenta *)

    fun tab(n) = 
        let
            fun loop (i) = if (i=n) then "" else "    "^loop(i+1)
        in 
            loop(0)
        end

    val level = ref 0
    fun change_indent (n) = 
        let 
            fun max (a, b) = if (a>b) then a else b
        in
            level := !(level) + n; level := max(!level, 0)
        end
    fun inc_level() = (change_indent(1); "")
    fun dec_level() = (change_indent(~1); "")
    fun indent () = tab(!level)

    fun compile p = print_str (ast_to_str p)
    and print_str s = TextIO.output (TextIO.stdOut, s)

    and ast_to_str (Expr exp) = (exp_to_str exp) ^ "\n" 
    | ast_to_str (Decs decs) = (decs_to_str decs) 

    and exp_to_str (NIL) = "nil"
    | exp_to_str (Int i) = digit (Int.toString i)
    | exp_to_str (Str s) = string ("\"" ^ s ^ "\"")
    | exp_to_str (FunctionCall f)  =    let
                                            val {Name, Args} = f
                                            val args = args_to_str Args
                                        in
                                            func Name ^ "(" ^ args ^ ")" 
                                        end

    | exp_to_str (MethodCall f)    =    let
                                            val {Obj, Name, Args} = f
                                            val obj_val = lval_to_str Obj
                                            val args_val = args_to_str Args
                                        in
                                            obj_val ^ "." ^ (func Name) ^ "(" ^ args_val ^ ")"
                                        end


    | exp_to_str (Oper oper)    =   let
                                        val (left, oper, right) = oper
                                        val operation = oper_to_str oper
                                    in
                                        (exp_to_str left) ^ operation ^ (exp_to_str right)
                                    end

    | exp_to_str (Lval l) = lval_to_str l
    | exp_to_str (Assign a)     =   let 
                                        val (Obj, Exp) = a
                                    in
                                        lval_to_str Obj ^ " := " ^ exp_to_str Exp 
                                    end

    | exp_to_str (Exps e) = exps_to_str e

    | exp_to_str (IfCond ie) =  let
                                    val {If, Then, Else} = ie
                                    
                                    val if_word = indent() ^ keyword "if "
                                    val then_word = keyword " then\n"
                                    val else_word = keyword (indent() ^ "else\n")

                                    val _ = inc_level()
                                    val then_ = indent() ^ exp_to_str Then
                                    val else_ = case Else of
                                        SOME (exp) => indent() ^ exp_to_str exp
                                        | NONE => ""
                                    val _ = dec_level()      
                                in
                                    case Else of
                                    SOME (exp) => "\n" ^ if_word ^ (exp_to_str If) ^ then_word ^ then_ ^ "\n" ^ else_word ^ (else_)
                                    | NONE => "\n" ^ if_word ^ (exp_to_str If) ^ then_word ^ then_
                                end

    | exp_to_str (While w) = let
                                val {Cond, Body} = w
                                
                                val while_word = keyword "while "
                                val do_word = keyword " do\n"

                                val _ = inc_level()
                                val do_ = indent() ^ exp_to_str Body
                                val _ = dec_level()
                            in
                                while_word ^ (exp_to_str Cond) ^ do_word ^ do_
                            end

    | exp_to_str (For f) =   let
                                val {Name, From, To, Body} = f
                                val for_word = keyword "for "
                                val do_word = keyword " do\n"
                                val to_word = keyword " to "
                                
                                val _ = inc_level()
                                val body = indent() ^ exp_to_str Body
                                val _ = dec_level()
                            in
                                for_word ^ (variable Name) ^ " := " ^ (exp_to_str From) ^ to_word ^ (exp_to_str To) ^ do_word ^ body   
                            end

    | exp_to_str Break = keyword "break\n"
    | exp_to_str (New t) = keyword "new " ^ t

    | exp_to_str (LetExp l) =    let
                                    val {Let, In} = l

                                    val let_word = keyword "let\n"
                                    val in_word = keyword (indent() ^ "in \n")
                                    val end_word = keyword (indent() ^ "end")

                                    val _ = inc_level()
                                    val let_ = decs_to_str Let
                                    val in_ = exps_to_str In
                                    val _ = dec_level()  
                                in
                                    let_word ^ let_ ^ in_word ^ in_ ^ "\n" ^ end_word 
                                end

    | exp_to_str (Array a) = let
                                val {Type, Len, Val} = a
                            in
                                type_ Type ^ "[" ^ (exp_to_str Len) ^ "] " ^ keyword "of " ^ (exp_to_str Val)
                            end

    | exp_to_str (Record r) = let
                                val {Type, Val} = r
                                val record_list = records_to_str Val
                            in
                                type_ Type ^ " { " ^ record_list ^ " } "
                            end  


    and record_to_str r = let
                                val {Key, Val} = r
                            in
                                variable Key ^ " = " ^ exp_to_str Val
                            end

    and records_to_str [] = ""
    |   records_to_str [x] = record_to_str x
    |   records_to_str (x::xs) = record_to_str x ^ ", " ^ records_to_str xs
                                                

    and args_to_str [] = ""
    | args_to_str [x]  = exp_to_str x
    | args_to_str (x::xs) = (exp_to_str x) ^ ", " ^ (args_to_str xs)

    and oper_to_str Plus = " + "
	| oper_to_str Minus = " - "
	| oper_to_str Mul   = " * "
	| oper_to_str Div   = " / "
	| oper_to_str Eq    = " = "
	| oper_to_str Neq   = " != "
	| oper_to_str Gt    = " > "
	| oper_to_str Lt    = " < "
	| oper_to_str Gte   = " >= "
	| oper_to_str Lte   = " <= "
	| oper_to_str And   = " & "
	| oper_to_str Or    = " | "


    and lval_to_str (Var v) = variable v
    | lval_to_str (Member obj) =    let
                                        val (Obj, Name) = obj
                                    in
                                        (lval_to_str Obj) ^ "." ^ (variable Name)
                                    end
    | lval_to_str (Ref a) = let
                                    val (Obj, index) = a
                                in
                                    (lval_to_str Obj) ^ "[" ^ (exp_to_str index) ^ "]"
                                end

    and dec_to_str (TypeDec t) =    let
                                        val {Name, Type} = t
                                        val type_word = keyword "type "
                                    in
                                        type_word ^ type_ Name ^ " = " ^ (type_to_str Type)
                                    end  

    | dec_to_str (VarDec v) =   let
                                    val {Name, Type, Val} = v
                                    val var_word = keyword "var "
                                in
                                    case Type of
                                    SOME (ty) => var_word ^ (variable Name) ^ " : " ^ keyword ty ^ " := " ^ (exp_to_str Val)
                                    | NONE => var_word ^ (variable Name) ^ " := " ^ (exp_to_str Val)

                                end
    | dec_to_str (FunDec f) =   let
                                    val {Name, ArgTypes, Type, Val} = f
                                    val _ = inc_level()
                                    val value_ = indent() ^ exp_to_str Val
                                    val keyword = keyword "function "
                                    val _ = dec_level()  
                                in
                                    case Type of
                                    SOME (ty) => keyword ^ (func Name) ^ "(" ^ (tyfields_to_str ArgTypes) ^ ")" ^ " : " ^ type_ ty ^ " = \n" ^  value_
                                    | NONE => keyword ^ (func Name) ^ "(" ^ (tyfields_to_str ArgTypes) ^ ")" ^ " = \n" ^ value_
                                end

    | dec_to_str (PrimitiveDec p) = let
                                        val {Name, ArgTypes, Type} = p
                                        val primitive_ = keyword "primitive "
                                    in
                                        case Type of
                                        SOME (ty) => primitive_ ^ (variable Name) ^ "(" ^ (tyfields_to_str ArgTypes) ^ ")" ^ " : " ^ type_ ty 
                                        | NONE => primitive_ ^ (variable Name) ^ "(" ^ (tyfields_to_str ArgTypes) ^ ")"
                                    end
                                    
    | dec_to_str (ClassDec c) =   let
                                        val {Name, Extends, Fields} = c
                                        val cfs = "[" ^  classfields_to_str Fields ^ "]"
                                    in
                                        case Extends of
                                        SOME e => ("class " ^ variable Name ^ ", extends " ^  e ^ ", {" ^ cfs ^ "}")
                                        | NONE => ("class " ^ variable Name ^ ", {" ^ cfs ^ "}")
                                    end

    and type_to_str (TypeAlias t) = type_ t
    |   type_to_str (RecordType r) = " { " ^ tyfields_to_str r ^ " } "
    |   type_to_str (ArrayType a) = (keyword "array of ") ^ (type_ a)
    | type_to_str (ClassType c) =   let
                                        val {Fields, Extends} = c
                                        val cfs = "[" ^ (classfields_to_str Fields) ^ "]"
                                    in
                                        case Extends of
                                        SOME e => ("class extends = " ^ e ^ ", {" ^ cfs ^ "}")
                                        | NONE => ("class {" ^ cfs ^ "}")
                                    end

    and tyfield_to_str (t) =    let
                                    val {ID, Type} = t
                                in
                                    variable ID ^ " : " ^ type_ Type
                                end

    and  tyfields_to_str [] = ""
    |   tyfields_to_str [x] = tyfield_to_str x
    | tyfields_to_str (x::xs) = tyfield_to_str x ^ ", " ^ tyfields_to_str xs

    and exps_to_str [] = ""
    | exps_to_str [x] = indent() ^ exp_to_str x 
    | exps_to_str (exps) = 
        let
            val _ = inc_level()
            fun expand [] = ""
                | expand (x::xs) = indent() ^ exp_to_str x ^ ";\n" ^ expand xs
            val final_str = expand exps 
            val _ = dec_level()
        in
            indent() ^ "(\n" ^ final_str ^ indent() ^ ")"
        end

    and decs_to_str [] = ""
        | decs_to_str (x :: xs) = indent() ^ (dec_to_str x) ^ "\n" ^ (decs_to_str xs)

    and classfields_to_str [] = ""
        | classfields_to_str ([x]) = (classfield_to_str x)
        | classfields_to_str (x::xs) = (classfield_to_str x) ^ ", " ^  (classfields_to_str xs)

    and classfield_to_str (ClassVarDec a) = let
                                                val {Name, Type, Val} = a
                                                val exp = exp_to_str Val
                                            in
                                                case Type of
                                                SOME t => (variable Name ^ ": " ^ type_ t ^ " = " ^ exp)
                                                | NONE => (variable Name ^ " = " ^ exp)
                                            end
        | classfield_to_str (ClassMethodDec m) = 
                                let
                                    val {Name, ArgTypes, Type, Val} = m
                                    val _ = inc_level()
                                    val value_ = indent() ^ exp_to_str Val
                                    val keyword = keyword "function "
                                    val _ = dec_level()  
                                in
                                    case Type of
                                    SOME (ty) => keyword ^ (func Name) ^ "(" ^ (tyfields_to_str ArgTypes) ^ ")" ^ " : " ^ type_ ty ^ " = \n" ^  value_
                                    | NONE => keyword ^ (func Name) ^ "(" ^ (tyfields_to_str ArgTypes) ^ ")" ^ " = \n" ^ value_
                                end
end