structure Env = RedBlackMapFn (struct
                        type ord_key = string
                        val compare = String.compare
                        end)

structure Translate =
struct
    exception Error
    exception StmConvError
    exception NotCJUMP
    exception whileCondNotRel

    exception NotSupported
    exception NotSupportedString
    exception NotSupportedBinop
    exception NotSupportedArray
    exception NotSupportedRecord
    exception NotSupportedDeclaration
    exception NotSupportedExpression
    exception NotSupportedLvalue

    exception NotDefined of string
    exception InvalidNumberOfArgs

    open Tiger;
    structure T = Tree;
    structure F = Frame;

    type 'a argmap = 'a Env.map
    val argenv: int list argmap ref = ref Env.empty
    fun insert_argenv (k, v) = (argenv := Env.insert (!argenv, k, v); !argenv)
    fun find_argenv (k) = (
          case Env.find (!argenv, k) of
                SOME(x) => x
              | _ => []
        )
    

    datatype exp =  Ex of T.expr     |
                    Nx of T.stm      |
                    Cx of int * int -> T.stm (* Temp.label * Temp.label *)

    (* push stack: sp = sp - wordsize*n *)
    fun pushstack n = T.MOVE (T.TEMP F.stackptr, 
                      T.BINOP (T.MINUS, T.TEMP F.stackptr, 
                              T.BINOP (T.MUL, T.CONST F.wordSize, T.CONST n)))

    (* pop stack: sp = sp + wordsize*n *)
    and popstack n = T.MOVE (T.TEMP F.stackptr, 
                      T.BINOP (T.PLUS, T.TEMP F.stackptr, 
                              T.BINOP (T.MUL, T.CONST F.wordSize, T.CONST n)))
    
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
                        T.ESEQ (s, _) => s              |
                        _ => raise StmConvError  )      |
        unNx (Nx s) = s                                 |
        unNx (Cx c) = unNx (Ex (unEx (Cx c)))
    
    fun unCx (Ex (T.CONST 0)) = (fn (t,f) => T.JUMP (T.NAME f, [f]))   |
        unCx (Ex (T.CONST 1)) = (fn (t,f) => T.JUMP (T.NAME t, [t]))   |
        unCx (Nx _) = raise Error   |
        unCx (Cx c) = c             |
        unCx _ = raise Error

    fun compile prog  = (
        case prog of
          Expr exp => T.EXP (exp_to_ir Env.empty exp)
        | Decs ds  => 
            let
              val (_, decs) = (decs_to_ir Env.empty ds)
            in
              unNx (Nx (T.list_to_SEQ decs))
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
            (
              case Val of
                Array (x) => raise NotSupportedArray
              | _ => 
                (
                    let
                      val t = Temp.newtemp()
                      val v = exp_to_ir env Val
                      val env_ = Env.insert(env, Name, t)
                    in
                      (env_, T.MOVE (T.TEMP t, v))
                    end
                )
              
            )
        | FunDec ({Name, ArgTypes, Type, Val}) => (
            let
              val result = Temp.newtemp()
              val return = Temp.newlabel()
              val _ = Env.insert (!argenv, Name, [])
              val env_ = 
                  let
                    fun extract_ID e ({ID, Type}) = 
                          let
                            val t = Temp.newtemp()
                            val _ = ( 
                                  let
                                    val xs = find_argenv(Name)
                                  in
                                    insert_argenv(Name, xs @ [t])
                                  end )
                          in
                            Env.insert (e, ID, t)
                          end
                    fun update_env e [] = e |
                        update_env e (x::xs) = update_env (extract_ID e x) xs
                  in
                    update_env Env.empty ArgTypes 
                  end
              val _ = insert_argenv(Name, return::(result::find_argenv(Name)))
              val body_ = exp_to_ir env_ Val
              (* val numOfVar = Env.numItems (env_) *)
              val lab = Temp.newlabel()
              val skip = Temp.newlabel()
              val env_ = Env.insert (env, Name, lab)
            in
              (env_, T.list_to_SEQ ([
                T.JUMP (T.NAME skip, [skip]),
                T.LABEL lab, 
                T.MOVE (T.TEMP result, body_),
                T.JUMP (T.NAME return, [return]),
                T.LABEL skip
              ]))
            end
        )
            
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
        | Assign x => assign_to_ir env x
        | LetExp le => letexp_to_ir env le
        | Exps es => exps_to_ir env es
        | IfCond x => ifcond_to_ir env x
        | For x => forloop_to_ir env x
        | While x => whileloop_to_ir env x
        | FunctionCall x => funcCall env x
        | Record x => raise NotSupportedRecord
        | Str _ => raise NotSupportedString
        |   _   => raise NotSupportedExpression
    )

    and funcCall env ({Name, Args}) = 
        let
          val lab = case Env.find (env, Name) of
                    SOME(x) => x
                  | _       => raise NotDefined Name
          (* func to convert all args to ir expr *)
          fun helper [] = []  | 
              helper (x::xs) = (exp_to_ir env x) :: (helper xs) 
          
          val exps_ = helper Args
          
          fun assign_regs (arg::args, exp::exps) = (T.MOVE(T.TEMP arg, exp)) :: (assign_regs (args, exps))  |
              assign_regs ([], []) = [] |
              assign_regs (_, _) = raise InvalidNumberOfArgs
          
          val return = List.hd (find_argenv(Name))
          val result = List.hd (List.tl (find_argenv(Name)))
          val assignments = T.list_to_SEQ (assign_regs(List.tl(List.tl(find_argenv(Name))), exps_))
        in
          T.ESEQ(T.list_to_SEQ([assignments, T.EXP (T.CALL (T.NAME lab, exps_)), T.LABEL return]), T.TEMP result) 
        end

    and whileloop_to_ir env ({Cond, Body})  = 
        let
          val cond_ = exp_to_ir env Cond

          val cond_ = case cond_ of
                        T.ESEQ(T.CJUMP(_), _) => cond_
                      | _ => raise whileCondNotRel

          val cond_ = unNx (Ex (cond_))
          
          val (t, f) = case cond_ of
                          T.CJUMP(_, _, _, t_, f_) => (t_, f_)
                        | _ => raise NotCJUMP

          val body_ = exp_to_ir env Body
          val seq = T.list_to_SEQ ([
            T.LABEL f,
            T.EXP body_,
            cond_,
            T.LABEL t
          ])
        in
          unEx (Nx seq)
        end

    and forloop_to_ir env ({Name, From, To, Body}) = 
        let
          val (env_, dec) = let
                              val t = Temp.newtemp()
                              val v = exp_to_ir env From
                              val e_ = Env.insert(env, Name, t)
                            in
                              (e_, T.MOVE (T.TEMP t, v))
                            end
          val loop = Temp.newlabel()
          val done = Temp.newlabel()
          val reg = case dec of
                      T.MOVE (T.TEMP t, v) => t
                    | _ => raise Error
          val inc = T.MOVE (T.TEMP reg, T.BINOP(T.PLUS, T.TEMP reg, T.CONST 1))
          val to_ = exp_to_ir env To
          val body_ = exp_to_ir env_ Body
          val seq = T.list_to_SEQ ([
            dec, T.LABEL loop, T.EXP(body_), inc, 
            T.CJUMP(T.EQ, T.TEMP reg, to_, done, loop),
            T.LABEL done
          ])
        in
          unEx (Nx (seq))
        end

    and ifcond_to_ir env ({If, Then, Else}) = 
        let
          val join = Temp.newlabel() (* once both if and else are done *)
          val if_ = unNx (Ex (exp_to_ir env If))
          val (t, f) = 
              case if_ of
                T.CJUMP(_, _, _, t_, f_) => (t_,f_)  (* getting the labels from cjump *)
              | _ => raise NotCJUMP

          val then_ = exp_to_ir env Then
          val else_ = case Else of  (* if no else then defaults to T.CONST 0 *)
            SOME (e) => exp_to_ir env e
          | _ => T.CONST 0

          val res = Temp.newtemp() (* final value is stored, either its then or else*)

          val stm = T.list_to_SEQ ([
            if_, T.LABEL t, T.MOVE(T.TEMP res, then_), (* the cjump in if_ will go to t or f, for true & false*)
            T.JUMP (T.NAME join, [join]), (* once the is done we jump to join (bypass else)*)
            T.LABEL f, T.MOVE (T.TEMP res, else_), T.LABEL join
          ])
        in
          T.ESEQ (stm, T.TEMP res)
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

    and assign_to_ir env (lval, exp) = (
        let
          val exp_ = exp_to_ir env exp
          val lval_ = lvalue_to_ir env lval
        in
          unEx (Nx (T.MOVE(lval_, exp_)))
        end
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
              unEx (Nx (T.CJUMP(oper_, e1_, e2_, t, f)))
            end
          )
      )
      end
end