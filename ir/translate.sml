structure Env = RedBlackMapFn (struct
                        type ord_key = string
                        val compare = String.compare
                        end)

structure Translate =
struct
    exception Error
    exception StmConvError
    exception NotCJUMP

    exception NotSupported
    exception NotSupportedString
    exception NotSupportedBinop
    exception NotSupportedDeclaration
    exception NotSupportedExpression
    exception NotSupportedLvalue
    exception NotDefined of string

    open Tiger;
    structure T = Tree;

    datatype exp =  Ex of T.expr     |
                    Nx of T.stm      |
                    Cx of int * int -> T.stm (* Temp.label * Temp.label *)
    
    (* Converting exp -> T.expr *)
    fun unEx (Ex e) =  e                            |
        unEx (Nx s) = T.ESEQ(s, T.CONST 0)          |
        unEx (Cx c) = 
            let 
                val r = Temp.newtemp()
                val t = Temp.newlabel() 
                val f = Temp.newlabel()
            in
                T.ESEQ(T.list_to_SEQ [
                            T.MOVE(T.TEMP r, T.CONST 1),
                            c (t, f),
                            T.LABEL f,
                            T.MOVE (T.TEMP r, T.CONST 0),
                            T.LABEL t
                            ], T.TEMP r)
            end  

    fun unNx (Ex e) =   (case e of
                        T.ESEQ(s, _) => s               | 
                        _ => raise StmConvError  )      |
        unNx (Nx s) = s                     |
        unNx (Cx c) = unNx (Ex (unEx (Cx c)))
    
    fun unCx (Ex (T.CONST 0)) = (fn (t,f) => T.JUMP (T.NAME f, [f]))   |
        unCx (Ex (T.CONST 1)) = (fn (t,f) => T.JUMP (T.NAME t, [t]))   |
        unCx (Nx _) = raise Error   |
        unCx (Cx c) = c             |
        unCx _ = raise Error

    fun compile prog  = (
        case prog of
          Expr exp => exp_to_ir Env.empty exp
        | Decs ds  => 
            let
              val (_, decs) = (decs_to_ir Env.empty ds)
            in
              unEx (Nx (T.list_to_SEQ decs))
            end
    )

    and decs_to_ir env [] = (env, [])  |
        decs_to_ir env [x] = 
            let
              val (env_, dec) = dec_to_ir env x
            in
              (env_, [dec])
            end    |
        decs_to_ir env (x::xs) = 
            let
              val (env_, dec) = dec_to_ir env x
              val (ev, decs) = decs_to_ir env_ xs
            in
              (ev, (dec :: decs))
            end

    and dec_to_ir env dec = (
        case dec of
          VarDec ({Name, Type, Val}) =>  
              let
                val t = Temp.newtemp()
                val v = exp_to_ir env Val
                val env_ = Env.insert(env, Name, t)
              in
                (env_, T.MOVE (T.TEMP t, v))
              end
        | _ => raise NotSupportedDeclaration
    )

    and exps_to_ir env xs = 
        let
          fun helper []       = T.CONST 0       |
              helper [x]      = exp_to_ir env x |
              helper (x::xs)  = T.ESEQ (T.EXP (exp_to_ir env x), helper xs)
        in
          helper xs
        end

    and exp_to_ir env exp = (
        case exp of
          NIL => T.CONST 0
        | Int i => T.CONST i
        | Oper x => binop_to_ir env x
        | Lval l => lvalue_to_ir env l
        | LetExp le => letexp_to_ir env le
        | Exps es => exps_to_ir env es
        | IfCond x => ifcond_to_ir env x
        | Str _ => raise NotSupportedString
        |   _   => raise NotSupportedExpression
    )

    and ifcond_to_ir env ({If, Then, Else}) = 
        let
          val join = Temp.newlabel()
          val if_ = unNx (Ex (exp_to_ir env If))
          val then_ = T.EXP (exp_to_ir env Then)
          val else_ = case Else of
            SOME (e) => T.EXP (exp_to_ir env e)
          | _ => T.EXP(T.CONST 0)

          val stm = T.list_to_SEQ ([
            if_, then_, else_
          ])
        in
          unEx (Nx stm)
        end

    and letexp_to_ir env ({Let, In}) = 
        let
          val (env_, decs) = decs_to_ir env Let
          val e = exps_to_ir env_ In
        in
          T.ESEQ (T.list_to_SEQ decs, e)
        end

    and lvalue_to_ir env lval = (
        case lval of
          Var (id) => (
              case Env.find(env, id) of
                SOME(t) => T.TEMP t
              | _ => raise NotDefined id
          ) 
        | _    => raise NotSupportedLvalue
    )

    and binop_to_ir env (e1, oper, e2) = 
      let
        val e1_ = exp_to_ir env e1
        val e2_ = exp_to_ir env e2
      in
      (
        case oper of
          Plus  => T.BINOP (T.PLUS, e1_, e2_)
        | Minus => T.BINOP (T.MINUS, e1_, e2_)
        | Mul   => T.BINOP (T.MUL, e1_, e2_)
        | Div   => T.BINOP (T.DIV, e1_, e2_)
        | And   => T.BINOP (T.AND, e1_, e2_)
        | Or    => T.BINOP (T.OR, e1_, e2_)
        | relop => 
          (
            let
              val t = Temp.newlabel()
              val f = Temp.newlabel()
              val oper_ = (
                case relop of
                      Eq => T.EQ
                    | Neq => T.NE
                    | Gt => T.GT
                    | Lt => T.LT
                    | Gte => T.GE
                    | Lte => T.LE
                    | _ => raise NotSupportedBinop
                )
            in
              unEx (Cx (fn (t, f) => T.CJUMP(oper_, e1_, e2_, t, f)))
            end
          )
      )
      end
end