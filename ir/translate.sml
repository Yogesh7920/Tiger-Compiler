structure Env = RedBlackMapFn (struct
                        type ord_key = string
                        val compare = String.compare
                        end)

structure Translate =
struct
    exception Error
    exception NotSupported of string
    exception NotDefined of string

    open Tiger;
    structure T = Tree;

    datatype exp =  Ex of T.expr     |
                    Nx of T.stm      |
                    Cx of int * int -> T.stm
    
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
                        T.ESEQ(s, temp) => s | 
                        _ => raise Error  )     |
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
        | Decs ds  => unEx (Nx (T.list_to_SEQ (decs_to_ir Env.empty ds)))
    )

    and decs_to_ir env [] = []  |
        decs_to_ir env [x] = 
            let
              val (env_, dec) = dec_to_ir env x
            in
              [dec]
            end    |
        decs_to_ir env (x::xs) = 
            let
              val (env_, dec) = dec_to_ir env x
            in
              dec :: (decs_to_ir env_ xs)
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
        | _ => raise NotSupported "Declaration"
    )

    and exp_to_ir env exp = (
        case exp of
          NIL => T.CONST 0
        | Int i => T.CONST i
        | Oper x => (binop_to_ir env x)
        | IfCond x => raise NotSupported "If Condition"
        | Lval l => lvalue_to_ir env l
        | Str _ => raise NotSupported "strings"
        |   _   => raise NotSupported "Expression"
    )

    and lvalue_to_ir env lval = (
        case lval of
          Var (id) => (
              case Env.find(env, id) of
                SOME(t) => T.TEMP t
              | _ => raise NotDefined id
          ) 
        | _    => raise NotSupported "lvalue"
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
        | _     => raise NotSupported "operator"
      )
      end
      
end