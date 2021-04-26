structure Translate =
struct
    datatype exp =  Ex of Tree.expr     |
                    Nx of Tree.stmt     |
                    Cx of Temp.label * Temp.label -> Tree.stmt
    
    fun unEx (Ex e) =  e   |
        unEx (Cx genstm) = 
            let 
                val r = Temp.newtemp()
                val t = Temp.newlabel() and f = Temp.newlabel()
            in
                Tree.ESEQ(SEQ[Tree.MOVE(Tree.TEMP r, Tree.CONST 1),
                                        genstm (t, f),
                                        Tree.LABEL f,
                                        Tree.MOVE (Tree.TEMP r, Tree.CONST 0),
                                        Tree.LABEL t], Tree.TEMP r)
            end         |
        unEx (Nx s) = Tree.ESEQ(s, Tree.CONST 0)
end