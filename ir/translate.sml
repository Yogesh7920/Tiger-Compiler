structure Translate =
struct
    exception Error

    datatype exp =  Ex of Tree.expr     |
                    Nx of Tree.stm      |
                    Cx of Temp.label * Temp.label -> Tree.stm
    
    fun unEx (Ex e) =  e                            |
        unEx (Nx s) = Tree.ESEQ(s, Tree.CONST 0)    |
        unEx (Cx c) = 
            let 
                val r = Temp.newtemp()
                val t = Temp.newlabel() 
                val f = Temp.newlabel()
            in
                Tree.ESEQ(SEQ[
                            Tree.MOVE(Tree.TEMP r, Tree.CONST 1),
                            c (t, f),
                            Tree.LABEL f,
                            Tree.MOVE (Tree.TEMP r, Tree.CONST 0),
                            Tree.LABEL t
                            ], Tree.TEMP r)
            end  

    fun ex_to_stm (Tree.ESEQ (s, temp)) = s |
        ex_to_stm _ = raise Error

    fun unNx (Ex e) = ex_to_stm e   |
        unNx (Nx s) = s             |
        unNx (Cx c) = unNx (Ex (unEx c))
    
    fun unCx (Ex (Tree.CONST 0)) = (fn (t,f) => Tree.JUMP (Tree.NAME f, [f]))   |
        unCx (Ex (Tree.CONST 1)) = (fn (t,f) => Tree.JUMP (Tree.NAME t, [t]))   |
        unCx (Nx _) = raise Error |
        unCx (Cx c) = c


    
end