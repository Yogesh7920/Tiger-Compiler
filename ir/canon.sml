structure IntMap = RedBlackMapFn (struct
                        type ord_key = int
                        val compare = Int.compare
                        end)

structure Canon = struct
    exception Error

    fun linearize (stm0 : Tree.stm) = 
        let
            fun helper (Tree.EXP (Tree.CONST _), s) = s |
                helper (s, Tree.EXP (Tree.CONST _)) = s |
                helper (s1, s2) = Tree.SEQ (s1, s2)

            fun commute (Tree.EXP (Tree.CONST _), _) = true |
                commute (_, Tree.NAME _)   = true |
                commute (_, Tree.CONST _)  = true |
                commute _ = false

            fun reorder ((Tree.CALL e) :: es) = 
                let
                    val r = Temp.newtemp()
                in
                    reorder (Tree.ESEQ (Tree.MOVE (Tree.TEMP r, (Tree.CALL e)), Tree.TEMP r) :: es)
                end |
                reorder (e :: es) = 
                    let
                        val (stms, e) = do_exp e
                        val (stms_, el) = reorder es
                    in
                        if commute(stms_, e)
                        then (helper (stms, stms_), e :: el)
                        else
                            let
                                val r = Temp.newtemp()
                            in
                                (helper(helper(stms, Tree.MOVE (Tree.TEMP r, e)), stms_), Tree.TEMP r :: el)
                            end
                    end |
                reorder [] = (Tree.EXP (Tree.CONST 0), [])

            and reorder_exp (el, build) = 
                                        let
                                            val (stms, el_) = reorder el
                                        in
                                            (stms, build el_)
                                        end
            and reorder_stm (el, build) = 
                                        let
                                            val (stms, el_) = reorder el
                                        in
                                            helper(stms, build el_)
                                        end
            (* Most places in do_exp & do_smt will have raise errors to avoid pat matching warning as 
                We know that only the req. pat will be passed *)
            and do_exp (Tree.BINOP(oper, a, b)) = reorder_exp ([a, b], fn [a, b] => Tree.BINOP (oper, a, b)  | _ => raise Error)    |
                do_exp (Tree.MEM(a)) = reorder_exp([a], fn [a] => Tree.MEM(a) | _ => raise Error)                                  |
                do_exp (Tree.ESEQ(s, e)) = 
                                        let
                                            val stms = do_stm s
                                            val (stms_, e) = do_exp e
                                        in
                                            (helper(stms_, stms), e)
                                        end |
                do_exp (Tree.CALL (e, el)) = reorder_exp (e :: el, fn e :: el => Tree.CALL (e, el) | _ => raise Error)     |
                do_exp e        = reorder_exp ([], fn [] => e | _ => raise Error)

            and do_stm (Tree.JUMP(e, labs)) = reorder_stm ([e], fn [e] => Tree.JUMP(e, labs) | _ => raise Error)    |
                do_stm (Tree.CJUMP(p, a, b, t, f)) = reorder_stm ([a, b], fn [a, b] => Tree.CJUMP(p, a, b, t, f) | _ => raise Error)    |
                do_stm (Tree.MOVE (Tree.TEMP t, Tree.CALL (e, el))) = reorder_stm (e :: el, fn e :: el => Tree.MOVE (Tree.TEMP t, Tree.CALL (e, el)) | _ => raise Error) |
                do_stm (Tree.MOVE (Tree.TEMP t, b)) = reorder_stm ([b], fn [b] => Tree.MOVE (Tree.TEMP t, b) | _ => raise Error) |
                do_stm (Tree.MOVE (Tree.MEM e, b)) = reorder_stm ([e, b], fn [e, b] => Tree.MOVE(Tree.MEM e, b) | _ => raise Error)  |
                do_stm (Tree.MOVE (Tree.ESEQ (s, e), b)) = do_stm (Tree.SEQ (s, Tree.MOVE (e, b)))  |
                do_stm (Tree.SEQ(a, b)) = helper(do_stm a, do_stm b)    |
                do_stm (Tree.EXP e) = reorder_stm ([e], fn [e] => Tree.EXP e | _ => raise Error) |
                do_stm s = reorder_stm ([], fn [] => s | _ => raise Error)


            fun linear (Tree.SEQ(a, b), l)  = linear(a, linear(b, l)) |
                linear(s, l)               = s :: l
        in
            linear(do_stm stm0, [])
        end

    fun basicBlocks stms = 
        let 
            val done = Temp.newlabel()
            (* bs : list list is the final output holding list of blocks. *)
            fun blocks ((Tree.LABEL l)::tail, bs) = 
                    let
                        fun next ((Tree.JUMP j)::xs, b) = last (xs, (Tree.JUMP j)::b)       (* block over *)    |   
                            next ((Tree.CJUMP cj)::xs, b) = last (xs, (Tree.CJUMP cj)::b)   (* block over *)    |   
                            next (stms as (Tree.LABEL lab :: _), b) = next (Tree.JUMP (Tree.NAME lab, [lab]) :: stms, b)  (* add Jump to prev. block*)  | 
                            next (s::xs, b) = next (xs, s::b)   |
                            next ([], b) = next ([Tree.JUMP (Tree.NAME done, [done])], b)   (* Last block *)
                        and last (stms, b) = blocks (stms, List.rev b :: bs) (* one block done *)
                    in
                        next (tail, [Tree.LABEL l])
                    end |
                blocks ([], bs) = List.rev bs  |
                blocks (stms, bs) = blocks (Tree.LABEL (Temp.newlabel ()) :: stms, bs) (* No label after jump -> create label and add*)
        in
            (blocks (stms, []), done) 
        end

    fun traceSchedule (blks, done) =  
        let

            fun addBlock (b as (Tree.LABEL l :: _), hm) = IntMap.insert (hm, l, b)   |
                addBlock (_, hm) = hm

            fun sepLast [x] = ([], x)   |
                sepLast (x::xs) = 
                        let
                            val (hs, l) = sepLast xs
                        in
                            (x::hs, l)
                        end |
                sepLast [] = raise Error
            (*  bs has the other blocks
                hm stands for hashmap *)
            fun trace (hm, b as (Tree.LABEL lab :: _), bs) = 
                let
                    val hm = IntMap.insert (hm, lab, [])
                    val (hs, l) = sepLast b

                    fun trace_helper (hs, Tree.JUMP (Tree.NAME lab, _)) = 
                            (
                                case (IntMap.find (hm, lab)) of
                                    SOME(b_) => hs @ trace (hm, b_, bs)   | 
                                    _ => b @ getnext (hm, bs)
                            )   |

                        trace_helper (hs, Tree.CJUMP (oper, x, y, t, f)) =    
                            (
                                case (IntMap.find(hm, t), IntMap.find(hm, f)) of

                                    (_, SOME(b_)) => b @ trace(hm, b_, bs)      | 
                                    (SOME(b_), _) => hs @ [Tree.CJUMP (Tree.notRelop oper, x, y, f, t)] @ trace (hm, b_, bs) 
                                                            (* Since only false label can be added below*) | 
                                    (_) => 
                                        let
                                            val r = Temp.newlabel()
                                        in
                                            hs @ [Tree.CJUMP (oper, x, y, t, r), Tree.LABEL r, Tree.JUMP (Tree.NAME f, [f])] @ getnext (hm, bs)
                                        end
                            )    |

                        trace_helper (hs, Tree.JUMP _) = b @ getnext (hm, bs)   |
                        trace_helper _ = raise Error (* Since basic Blocking is done before every block must end with a jump*)
                in
                    trace_helper(hs, l)
                end |
                trace (_, _, _) = raise Error (* Since basic Blocking is done before every block must start with a label*)
            (* Find the next block (from already seen) to trace *)
            and getnext (hm, (b as (Tree.LABEL lab :: _))::bs) = 
                                    (case IntMap.find (hm, lab) of
                                        SOME(_) => trace (hm, b, bs)    |
                                        _ => getnext (hm, bs))          |
                getnext (hm, []) = []   |
                getnext _ = raise Error (* This fun is called only with the hashmap *)
        in
            getnext (List.foldr addBlock IntMap.empty blks, blks) @ [Tree.LABEL done]
        end

    fun canonize prog = traceSchedule (basicBlocks (linearize prog))
end