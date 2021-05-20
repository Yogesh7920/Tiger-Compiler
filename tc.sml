(* The compiler from Tigeression to rp *)
structure TIGER =
struct

structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
structure TigerLex    = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerParser = Join( structure ParserData = TigerLrVals.ParserData
			     structure Lex        = TigerLex
			     structure LrParser   = LrParser
			   )

(* 
At this point every thing regarding lexing and parsing is contained in
the TigerParser structure. Let us create a lexer using this.
*)
(* Build Lexers *)
fun makeTigerLexer strm = TigerParser.makeLexer (fn n => TextIO.inputN(strm,n))
val makeFileLexer      = makeTigerLexer o TextIO.openIn


fun print_str s = TextIO.output (TextIO.stdOut, s)
fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr,
					    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")


(* Parse command line and set a suitable lexer *)

fun arg_parser 	[]  = 	([], [])	|
    arg_parser  xs	=	List.partition (fn x => (
		(x="--pp") orelse (x="--ast") orelse 
		(x="--ir") orelse (x="--can") orelse
		(x="--bb") orelse (x="--lin")
		)) xs

fun element_exists (item, xs) = List.exists (fn x => (x=item)) xs;

val args = CommandLine.arguments()
val (flag, file) = arg_parser args

val pp = element_exists ("--pp", flag)
val ast = element_exists ("--ast", flag)
val ir = element_exists ("--ir", flag)
val cflag = element_exists ("--can", flag)
val bb = element_exists ("--bb", flag)
val lin = element_exists ("--lin", flag)
val default = not(pp) andalso not(ast) andalso not(ir) andalso not(cflag) andalso not(bb) andalso not(lin)

val thisLexer = case file of
			[]  => makeTigerLexer TextIO.stdIn
		|  [x] => makeFileLexer x
		|  _ 	=> (TextIO.output(TextIO.stdErr, "usage: ec file"); OS.Process.exit OS.Process.failure)


val (program,_) = TigerParser.parse (0,thisLexer,print_error,()) (* parsing *)

fun printCanon [] = ()  | 
	printCanon (x::xs) = (Printtree.printtree (TextIO.stdOut, x); print_str "\n"; printCanon xs; ())

val _ 	= if (ast) then (print_str "\n\027[1;37mAST\027[0m\n") else ()
val _	= if (ast) then (PrintAST.print_ast program) else [()]

val _ = if (pp) then (print_str "\n\027[1;37mPretty-Print\027[0m\n\n") else ()
val _ = if (pp) then (PP.compile program) else ()

val tree = if (ir orelse cflag orelse bb orelse lin orelse default) then Translate.compile program else Tree.EXP(Tree.CONST 0)

val _ = if (ir) then (print_str "\n\027[1;37mIntermediate-Representation \027[0m\n\n") else ()
val _ = if (ir) then (Printtree.printtree (TextIO.stdOut, tree)) else ()

val lin_tree = if (lin) then Canon.canonize_uptoLin tree else [tree]


val _ = if (lin) then (print_str "\n\027[1;37mIntermediate-Representation (canonized upto linearize) \027[0m\n\n") else ()
val _ = if (lin) then (printCanon (lin_tree)) else ()


val bb_tree = if (bb) then Canon.canonize_uptoBB tree else [[tree]]
val bb_count = List.length bb_tree

fun block_show (x: Tree.stm list): unit = (printCanon x; print_str "\n\nBLOCK-DONE---\n\n")
val _ = if (bb) then (print_str "\n\027[1;37mIntermediate-Representation (canonized upto basic blocks) \027[0m\n\n") else ()
val _ = if (bb) then (List.map block_show bb_tree) else [()]
val _ = if (bb) then (print_str ("Number of Blocks = " ^ Int.toString bb_count ^ "\n")) else ()

val tree = if (cflag) then Canon.canonize tree else [tree]



val _ = if (cflag) then (print_str "\n\027[1;37mIntermediate-Representation (canonized) \027[0m\n\n") else ()
val _ = if (cflag) then (printCanon (tree)) else ()

(* default *)
val _ = if (default) then (print_str "\n\027[1;37mIntermediate-Representation\027[0m\n\n") else ()
val _ = if (default) then (printCanon (tree)) else ()

end
