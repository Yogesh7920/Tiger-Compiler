type lineNo            = int
type pos               = lineNo  (* The type of Should match with expr.yacc *)
val  lineRef : pos ref = ref 0   (* reference variable to keep track of position.
                                            Typing not necessary just for clarity *)

fun updateLine n      = lineRef := !(lineRef) + n


type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token

fun lineRange l r = "line " ^ l
fun err pos msg = print msg;

fun eof   ()      = Tokens.EOF (!lineRef,!lineRef)

fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt        = toSigned o String.explode

val newlineCount = List.length o List.filter (fn x => x = #"\n") o String.explode

(* \n({ws}*\n)*    => (    let val old = !lineRef
                        in updateLine (newlineCount yytext); Tokens.NEWLINE (old, !lineRef)
                        end
                    ); *)

%%

%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS));
ws    = [\ \t];
digit = [0-9];
digits = {digit}+;
alpha = [a-zA-Z];
id = {alpha}({alpha}|{digit}|"_")*;
strings = "\""(\\.|[^\\"])*"\"";
%s COMMENT;

%%

<INITIAL,COMMENT> \n        =>  (updateLine 1; lex());
<INITIAL,COMMENT> "/*"      =>  (YYBEGIN COMMENT; lex ());
<COMMENT> "*/"              =>  (YYBEGIN INITIAL; lex ());
<COMMENT> .                 =>  (lex());

<INITIAL> {ws}+             =>  ( lex() );

<INITIAL> "array"         => ( Tokens.ARRAY (!lineRef, !lineRef));
<INITIAL> "if"            => ( Tokens.IF (!lineRef, !lineRef));
<INITIAL> "then"          => ( Tokens.THEN (!lineRef, !lineRef));
<INITIAL> "else"          => ( Tokens.ELSE (!lineRef, !lineRef));
<INITIAL> "while"         => ( Tokens.WHILE (!lineRef, !lineRef));
<INITIAL> "for"           => ( Tokens.FOR (!lineRef, !lineRef));
<INITIAL> "to"            => ( Tokens.TO (!lineRef, !lineRef));
<INITIAL> "do"            => ( Tokens.DO (!lineRef, !lineRef));
<INITIAL> "let"           => ( Tokens.LET (!lineRef, !lineRef));
<INITIAL> "in"            => ( Tokens.IN (!lineRef, !lineRef));
<INITIAL> "end"           => ( Tokens.END (!lineRef, !lineRef));
<INITIAL> "of"            => ( Tokens.OF (!lineRef, !lineRef));
<INITIAL> "break"         => ( Tokens.BREAK (!lineRef, !lineRef));
<INITIAL> "nil"           => ( Tokens.NIL (!lineRef, !lineRef));
<INITIAL> "function"      => ( Tokens.FUNCTION (!lineRef, !lineRef));
<INITIAL> "var"           => ( Tokens.VAR (!lineRef, !lineRef));
<INITIAL> "type"          => ( Tokens.TYPE (!lineRef, !lineRef));
<INITIAL> "import"        => ( Tokens.IMPORT (!lineRef, !lineRef));
<INITIAL> "primitive"     => ( Tokens.PRIMITIVE (!lineRef, !lineRef));
<INITIAL> "class"         => ( Tokens.CLASS (!lineRef, !lineRef));
<INITIAL> "new"           => ( Tokens.NEW (!lineRef, !lineRef));
<INITIAL> "method"        => ( Tokens.METHOD (!lineRef, !lineRef));

<INITIAL> ";"          => ( Tokens.SEMICOLON (!lineRef, !lineRef));
<INITIAL> "("          => ( Tokens.LPARAN (!lineRef, !lineRef));
<INITIAL> ")"          => ( Tokens.RPARAN (!lineRef, !lineRef));
<INITIAL> "["          => ( Tokens.LBRACK (!lineRef, !lineRef));
<INITIAL> "]"          => ( Tokens.RBRACK (!lineRef, !lineRef));
<INITIAL> "{"          => ( Tokens.LCURLY (!lineRef, !lineRef));
<INITIAL> "}"          => ( Tokens.RCURLY (!lineRef, !lineRef));
<INITIAL> "."          => ( Tokens.DOT (!lineRef, !lineRef));
<INITIAL> "+"          => ( Tokens.PLUS (!lineRef, !lineRef));
<INITIAL> "-"          => ( Tokens.MINUS (!lineRef, !lineRef));
<INITIAL> "*"          => ( Tokens.MUL (!lineRef, !lineRef));
<INITIAL> "/"          => ( Tokens.DIV (!lineRef, !lineRef));
<INITIAL> "<>"         => ( Tokens.NEQ (!lineRef, !lineRef));
<INITIAL> "<="         => ( Tokens.LTE (!lineRef, !lineRef));
<INITIAL> "<"          => ( Tokens.LT (!lineRef, !lineRef));
<INITIAL> ">="         => ( Tokens.GTE (!lineRef, !lineRef));
<INITIAL> ">"          => ( Tokens.GT (!lineRef, !lineRef));
<INITIAL> "&"          => ( Tokens.AND (!lineRef, !lineRef));
<INITIAL> "|"          => ( Tokens.OR (!lineRef, !lineRef));
<INITIAL> ":="         => ( Tokens.ASSIGN (!lineRef, !lineRef));
<INITIAL> "="          => ( Tokens.EQ (!lineRef, !lineRef));
<INITIAL> ":"          => ( Tokens.COLON (!lineRef, !lineRef));
<INITIAL> ","          => ( Tokens.COMMA (!lineRef, !lineRef));

<INITIAL> {digits}      => ( Tokens.INT (toInt yytext, !lineRef, !lineRef));
<INITIAL> {id}          => ( Tokens.ID (yytext, !lineRef, !lineRef));
<INITIAL> {strings}     => ( Tokens.STR(String.substring(yytext, 1, String.size(yytext) - 2), yypos, yypos + size(yytext)));
.                       => (err yypos ("Invalid !! '" ^ yytext ^ "' found"); lex());