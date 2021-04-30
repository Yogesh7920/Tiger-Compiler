structure Translate =
struct
    exception Error

    datatype exp =  Ex of Tree.expr     |
                    Nx of Tree.stm      |
                    Cx of int * int -> Tree.stm
    
    (* Converting exp -> Tree.expr *)
    fun unEx (Ex e) =  e                            |
        unEx (Nx s) = Tree.ESEQ(s, Tree.CONST 0)    |
        unEx (Cx c) = 
            let 
                val r = Temp.newtemp()
                val t = Temp.newlabel() 
                val f = Temp.newlabel()
            in
                Tree.ESEQ(Tree.list_to_SEQ [
                            Tree.MOVE(Tree.TEMP r, Tree.CONST 1),
                            c (t, f),
                            Tree.LABEL f,
                            Tree.MOVE (Tree.TEMP r, Tree.CONST 0),
                            Tree.LABEL t
                            ], Tree.TEMP r)
            end  

    fun unNx (Ex e) =   (case e of
                        Tree.ESEQ(s, temp) => s | 
                        _ => raise Error  )     |
        unNx (Nx s) = s                     |
        unNx (Cx c) = unNx (Ex (unEx (Cx c)))
    
    fun unCx (Ex (Tree.CONST 0)) = (fn (t,f) => Tree.JUMP (Tree.NAME f, [f]))   |
        unCx (Ex (Tree.CONST 1)) = (fn (t,f) => Tree.JUMP (Tree.NAME t, [t]))   |
        unCx (Nx _) = raise Error   |
        unCx (Cx c) = c             |
        unCx _ = raise Error


    
end