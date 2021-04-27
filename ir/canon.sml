structure Canon = struct
    type stms = Tree.stm list
    exception Error

    fun linearize Tree.stm s = 
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
                    reorder (Tree.SEQ (Tree.MOVE (Tree.TEMP r, (Tree.CALL e)) Tree.TEMP r) :: es)
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
                                (helper(helper(stms, Tree.MOVE (Tree.TEMP t, e)), stms_). Tree.TEMP t :: el)
                            end
                    end |
                reorder [] = (Tree.EXP (Tree.CONST 0), [])

            fun reorder_exp (el, build) = 
                                        let
                                            (stms, el_) = reorder el
                                        in
                                            (stms, build el_)
                                        end
            fun reorder_stm (el, build) = 
                                        let
                                            (stms, el_) = reorder el
                                        in
                                            helper(stms, build el_)
                                        end

            fun do_exp (Tree.BINOP(p, a, b)) = reorder_exp([a, b], fn [a, b] => Tree.BINOP(p, a, b))    |
                do_exp (Tree.MEM(a)) = reorder_exp([a], fn [a] => Tree.MEM(a))                          |
                do_exp (Tree.ESEQ(s, e)) = 
                                        let
                                            val stms = do_stm s
                                            val (stms_, e) = do_exp e
                                        in
                                            (helper(stms_, stms), e)
                                        end |
                do_exp (Tree.CALL (e, el)) = reorder_exp (e :: el, fn e :: el => Tree.CALL (e, el))     |
                do_exp e        = reorder_exp ([], fn [] => e)

            fun do_stm (Tree.JUMP(e, labs)) = reorder_stm ([e], fn [e] => Tree.JUMP(e, labs))    |
                do_stm (Tree.CJUMP(p, a, b, t, f)) = reorder_stm ([a, b], fn [a, b] => Tree.CJUMP(p, a, b, t, f))    |
                do_stm (Tree.MOVE (Tree.TEMP t, b)) = reorder_stm ([b], fn [b] => Tree.MOVE(Tree.TEMP t, b)) |
                do_stm (Tree.MOVE (Tree.TEMP t, Tree.CALL (e, el))) = reorder_stm (e :: el, fn e :: el => Tree.MOVE (Tree.TEMP t, Tree.CALL (e, el))) |
                do_stm (Tree.MOVE (Tree.MEM e, b)) = reorder_stm ([e, b], fn [e, b] => Tree.MOVE(Tree.MEM e, b))  |
                do_stm (Tree.MOVE (Tree.ESEQ (s, e), b)) = do_stm (Tree.SEQ (s, Tree.MOVE (e, b)))  |
                do_stm (Tree.SEQ(a, b)) = helper(do_stm a, do_stm b)    |
                do_stm (T.EXP e) = reorder_stm ([e], fn [e] => T.EXP e) |
                do_stm s = reorder_stm ([], fn [] => s)


            fun linear (Tree.SEQ(a, b), l)  = linear(a, linear(b, l)) |
                linear(s, l)               = s :: l
        in
            linear(do_stm s, [])
        end

    fun basicBlocks stms = 
        let 
            val r = Temp.newlabel ()
            fun blocks ((Tree.LABEL l)::tail, bs) = 
                    let
                        fun next ((Tree.JUMP j)::xs, b) = last (xs, (Tree.JUMP j)::b)    |
                            next ((Tree.CJUMP cj)::xs, b) = last (xs, (Tree.CJUMP cj)::b)   |
                            next (stms as (Tree.LABEL lab :: _), b) = next (Tree.JUMP (Tree.NAME lab, [lab]) :: stms, b)  |
                            next (s::xs, b) = next (xs, s::b)   |
                            next ([], b) = next ([Tree.JUMP (Tree.NAME r, [r])], b)
                        and last (stms, b) = blocks (stms, List.rev b :: bs)
                    in
                        next (tail, [Tree.LABEL l])
                    end |
                blocks ([], bs) = List.rev bs  |
                blocks (stms, bs) = blocks (Tree.LABEL (Temp.newlabel ()) :: stms, bs)
        in
            (blocks (stms, []), r) 
        end

    fun traceSchedule (blocks, done) =  
        let
            fun get (item, xs) = List.find (fn x => (x=item)) xs
            fun addBlock (b as (Tree.LABEL s :: _), xs) = b :: xs
            fun sepLast [x] = ([], x)   |
                sepLast (x::xs) = 
                        let
                            val (hs, l) = sepLast xs
                        in
                            (x::hs, l)
                        end
            
            fun trace (xs, b as (Tree.LABEL lab :: _), bs) = 
                let
                    val xs_ = lab :: xs
                    val (hs, l) = sepLast b
                    fun trace_helper (hs, Tree.JUMP (Tree.NAME lab, _)) = 
                                        (case (get (lab, xs_)) of
                                            SOME (b_) => hs @ trace (xs_, b_, bs)   | 
                                            _ => b @ getnext (xs_, bs))             |
                        trace_helper (hs, Tree.CJUMP (op, x, y, t, f)) =    
                                        (case (get (t, xs_), get (f, xs_)) of
                                            (_, SOME(b_)) => b @ trace(xs, b_, bs)  | 
                                            (SOME(b_), _) => hs @ [Tree.CJUMP (Tree.notRelop op, x, y, f, t)] @ trace (xs_, b_, bs) |
                                            (_) => 
                                                let
                                                    val r = Temp.newlabel()
                                                in
                                                    hs @ [Tree.CJUMP (op, x, y, t, r), Tree.LABEL r, Tree.JUMP (Tree.NAME f, [f])] @ getnext (xs_, bs)
                                                end)    |
                        trace_helper (hs, Tree.JUMP _) = b @ getnext (xs_, bs)
                in
                    trace_helper(hs, l)
                end

            and getnext (xs, (b as Tree.LABEL lab :: _))::bs = 
                                    (case get(lab, xs) of
                                        SOME(_) => trace (xs, b, bs)    |
                                        _ => getnext (xs, bs))          |
                getnext (xs, []) = []
        in
            getnext (List.foldr addBlock [] blocks, blocks) @ [Tree.LABEL done]
        end
end