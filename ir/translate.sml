structure Env = RedBlackMapFn (struct
                        type ord_key = string
                        val compare = String.compare
                        end)

structure Translate =
struct
    exception UnknownError
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
    val argenv: int argmap ref = ref Env.empty
    fun insert_argenv (k, v) = (argenv := Env.insert (!argenv, k, v); !argenv)
    fun find_argenv (k) = (
          case Env.find (!argenv, k) of
                SOME(x) => x
              | _ => raise NotDefined k
        )

    datatype exp =  Ex of T.expr     |
                    Nx of T.stm      |
                    Cx of int * int -> T.stm (* Temp.label * Temp.label *)

    (* push stack: sp = sp - wordsize*n *)
    fun pushstack 1 = T.MOVE (T.TEMP F.stackptr, 
                      T.BINOP (T.MINUS, T.TEMP F.stackptr, T.CONST F.wordSize))  |
        pushstack n = T.MOVE (T.TEMP F.stackptr, 
                      T.BINOP (T.MINUS, T.TEMP F.stackptr, 
                              T.BINOP (T.MUL, T.CONST F.wordSize, T.CONST n)))

    (* pop stack: sp = sp + wordsize*n *)
    and popstack 1 = T.MOVE (T.TEMP F.stackptr, 
                      T.BINOP (T.PLUS, T.TEMP F.stackptr, T.CONST F.wordSize))  |
        popstack n = T.MOVE (T.TEMP F.stackptr, 
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
        unCx (Nx _) = raise UnknownError   |
        unCx (Cx c) = c             |
        unCx _ = raise UnknownError

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
                      val env_ = Env.insert(env, Name, T.TEMP t)
                    in
                      (env_, T.MOVE (T.TEMP t, v))
                    end
                )
              
            )
        | FunDec ({Name, ArgTypes, Type, Val}) => (
            let
              val lab = Temp.newlabel()
              val env_fun = Env.insert (env, Name, T.NAME lab)

              val numOfArgs = List.length ArgTypes
              val _ = insert_argenv(Name, numOfArgs)

              val return = T.MEM(T.BINOP(T.MINUS, T.TEMP F.frameptr, T.CONST 1))
              val assign_retAddress = T.MOVE(T.TEMP F.retaddr, return)
              val _ = Env.insert (!argenv, Name, numOfArgs)
              val env_ = 
                  let
                    fun extract_ID e ({ID, Type}) = 
                          let
                            val t = Temp.newtemp()
                          in
                            Env.insert (e, ID, T.TEMP t)
                          end
                    fun update_env e [] = e |
                        update_env e (x::xs) = update_env (extract_ID e x) xs
                  in
                    update_env Env.empty ArgTypes 
                  end
              val regs = Env.listItems(env_)
              val assign_reg = 
                    let
                      fun extract (0, []) = []  |
                          extract (n, (x::xs)) = T.MOVE (x, T.MEM (
                            T.BINOP(T.MINUS, T.TEMP F.frameptr, T.CONST (numOfArgs-n+2))
                            )) :: extract ((n-1), xs) |
                          extract (_, _) = raise InvalidNumberOfArgs
                    in
                      extract (numOfArgs, regs)
                    end

              val env_ = Env.insert(env_, Name, T.NAME lab)
              val env_ = Env.unionWith (fn (x, y) => y) (env, env_)
              val body_ = exp_to_ir env_ Val

              val skip = Temp.newlabel()
              val restore_fp = T.MOVE (T.TEMP F.frameptr, T.MEM(T.TEMP F.frameptr))
              val pop_stack = popstack (numOfArgs+2) (* pop arg, ret addr, prev fp*)
            in
              (env_fun, T.list_to_SEQ ([
                T.JUMP (T.NAME skip, [skip]),
                T.LABEL lab] 
                @ assign_reg @
                [T.MOVE (T.TEMP F.retval, body_),
                assign_retAddress,
                restore_fp,
                pop_stack,
                T.JUMP (T.TEMP F.retaddr, [F.retaddr]),
                T.LABEL skip
              ]))
            end
        )
            
        | _ => raise NotSupportedDeclaration
    )

    and exps_to_ir env xs = 
        let
          val exps = List.map (exp_to_ir env) xs
          val last = List.hd (List.rev exps)
          val hs = List.rev (List.tl (List.rev exps))
          val hs = List.map (T.EXP) hs
        in
          if (hs=[]) then T.ESEQ(T.EXP(T.CONST 0), last) else T.ESEQ (T.list_to_SEQ(hs), last)
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
          val locals = 
              let
                fun fil (T.TEMP(x)) = true  |
                    fil _   = false
              in
                List.filter fil (Env.listItems(env))
              end
          val num_of_locals = List.length(locals)
          val alloc_local = if (num_of_locals>0) then [pushstack(num_of_locals)] else []
          val save_local = 
                let
                  fun helper (0, []) = []  |
                      helper (n, (x::xs)) = if (n=num_of_locals) then (
                        T.MOVE (T.MEM (T.TEMP F.stackptr), x) :: helper ((n-1), xs)
                      ) else (
                        T.MOVE (T.MEM (
                        T.BINOP(T.PLUS, T.TEMP F.stackptr, T.CONST (num_of_locals-n))
                        ), x) :: helper ((n-1), xs)
                        ) |
                      helper (_, _) = raise UnknownError
                in
                  helper (num_of_locals, locals)
                end
          val lab = case Env.find(env, Name) of
                            SOME(x) => x
                          | _ => raise NotDefined Name
          val numOfArgs = find_argenv(Name)
          val _ = if (numOfArgs=List.length Args) then numOfArgs else raise InvalidNumberOfArgs
          val push_for_prev_fp = pushstack(1)
          val add_prev_fp = T.MOVE(T.MEM(T.TEMP F.stackptr), T.TEMP F.frameptr)
          val assign_new_fp = T.MOVE (T.TEMP F.frameptr, T.TEMP F.stackptr)
          val retAddress = Temp.newlabel()
          val push_for_ret_and_args = pushstack(1+numOfArgs)
    
          val add_retAddress = T.MOVE(T.MEM(T.BINOP(T.MINUS, T.TEMP F.frameptr, T.CONST 1)), T.NAME retAddress)
          val args = List.map (exp_to_ir env) Args
          val add_args = 
              let
                fun helper 0 [] = []  |
                    helper 1 [x] = [T.MOVE(T.MEM(T.BINOP(
                      T.MINUS, T.TEMP F.frameptr, T.CONST (numOfArgs+1)
                      )), x)]  |
                    helper n (x::xs) = T.MOVE(T.MEM(T.BINOP(
                      T.MINUS, T.TEMP F.frameptr, T.CONST (numOfArgs-n+2)
                      )), x) :: helper (n-1) xs |
                    helper _ _ = raise InvalidNumberOfArgs
              in
                helper numOfArgs args
              end
          
          val retrieve_locals = 
                let
                  fun helper (0, []) = []  |
                      helper (n, (x::xs)) = if (n=num_of_locals) then (
                        T.MOVE (x, T.MEM (T.TEMP F.stackptr)) :: helper ((n-1), xs)
                      ) else (
                        T.MOVE (x, T.MEM (
                        T.BINOP(T.MINUS, T.TEMP F.stackptr, T.CONST (num_of_locals-n))
                        )) :: helper ((n-1), xs)
                        ) |
                      helper (_, _) = raise InvalidNumberOfArgs
                in
                  helper (num_of_locals, locals)
                end
          val release_local_alloc = if (num_of_locals>0) then [popstack(num_of_locals)] else []
        in
          T.ESEQ(T.list_to_SEQ(
            alloc_local @ save_local @ [
            push_for_prev_fp, add_prev_fp, 
            assign_new_fp, 
            push_for_ret_and_args,
            add_retAddress
            ] @ add_args @ [
              T.EXP(T.CALL(lab, args)),
              T.LABEL retAddress
              ] @ retrieve_locals @ release_local_alloc), T.TEMP F.retval) 
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
                              val e_ = Env.insert(env, Name, T.TEMP t)
                            in
                              (e_, T.MOVE (T.TEMP t, v))
                            end
          val loop = Temp.newlabel()
          val done = Temp.newlabel()
          val reg = case dec of
                      T.MOVE (T.TEMP t, v) => t
                    | _ => raise UnknownError
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
          val _ = Printtree.printtree (TextIO.stdOut, T.EXP(exp_to_ir env If))
          val (pre_cond, if_) = case (exp_to_ir env If) of
                                  T.ESEQ(pre, e) => (pre, unNx (Ex e))
                                | _ => raise StmConvError
          val pre_cond = case pre_cond of
                          T.EXP(T.CONST _) => []
                        | T.EXP(T.ESEQ(s, _)) => T.SEQ_to_list(s)
                        | _ => raise UnknownError
          val (t, f) = 
              case if_ of
                T.CJUMP(_, _, _, t_, f_) => (t_,f_)  (* getting the labels from cjump *)
              | _ => raise NotCJUMP

          val then_ = exp_to_ir env Then
          val else_ = case Else of  (* if no else then defaults to T.CONST 0 *)
            SOME (e) => exp_to_ir env e
          | _ => T.CONST 0

          val res = Temp.newtemp() (* final value is stored, either its then or else*)

          val stm = T.list_to_SEQ (pre_cond @ [
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
                SOME(t) => t
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