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

%%

%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS));
ws    = [\ \t];
digits = [0-9]+;
alpha = [a-zA-Z];
id = {alpha}+;

%%

"#".*\n         => ( updateLine 1; lex ());
{ws}+           => ( lex() );
\n({ws}*\n)*    => (    let val old = !lineRef
                        in updateLine (newlineCount yytext); Tokens.NEWLINE (old, !lineRef)
                        end
                    );

"array"         => ( Tokens.ARRAY (!lineRef, !lineRef));
"if"            => ( Tokens.IF (!lineRef, !lineRef));
"then"          => ( Tokens.THEN (!lineRef, !lineRef));
"else"          => ( Tokens.ELSE (!lineRef, !lineRef));
"while"         => ( Tokens.WHILE (!lineRef, !lineRef));
"for"           => ( Tokens.FOR (!lineRef, !lineRef));
"to"            => ( Tokens.TO (!lineRef, !lineRef));
"do"            => ( Tokens.DO (!lineRef, !lineRef));
"let"           => ( Tokens.LET (!lineRef, !lineRef));
"in"            => ( Tokens.IN (!lineRef, !lineRef));
"end"           => ( Tokens.END (!lineRef, !lineRef));
"of"            => ( Tokens.OF (!lineRef, !lineRef));
"break"         => ( Tokens.BREAK (!lineRef, !lineRef));
"nil"           => ( Tokens.NIL (!lineRef, !lineRef));
"function"      => ( Tokens.FUNCTION (!lineRef, !lineRef));
"var"           => ( Tokens.VAR (!lineRef, !lineRef));
"type"          => ( Tokens.TYPE (!lineRef, !lineRef));
"import"        => ( Tokens.IMPORT (!lineRef, !lineRef));
"primitive"     => ( Tokens.PRIMITIVE (!lineRef, !lineRef));
"class"         => ( Tokens.CLASS (!lineRef, !lineRef));
"new"           => ( Tokens.NEW (!lineRef, !lineRef));
"method"        => ( Tokens.METHOD (!lineRef, !lineRef));

";"          => ( Tokens.SEMICOLON (!lineRef, !lineRef));
"("          => ( Tokens.LPARAN (!lineRef, !lineRef));
")"          => ( Tokens.RPARAN (!lineRef, !lineRef));
"["          => ( Tokens.LBRACK (!lineRef, !lineRef));
"]"          => ( Tokens.RBRACK (!lineRef, !lineRef));
"{"          => ( Tokens.LCURLY (!lineRef, !lineRef));
"}"          => ( Tokens.RCURLY (!lineRef, !lineRef));
"."          => ( Tokens.DOT (!lineRef, !lineRef));
"+"          => ( Tokens.PLUS (!lineRef, !lineRef));
"-"          => ( Tokens.MINUS (!lineRef, !lineRef));
"*"          => ( Tokens.MUL (!lineRef, !lineRef));
"/"          => ( Tokens.DIV (!lineRef, !lineRef));
"<>"         => ( Tokens.NEQ (!lineRef, !lineRef));
"<="         => ( Tokens.LTE (!lineRef, !lineRef));
"<"          => ( Tokens.LT (!lineRef, !lineRef));
">="         => ( Tokens.GTE (!lineRef, !lineRef));
">"          => ( Tokens.GT (!lineRef, !lineRef));
"&"          => ( Tokens.AND (!lineRef, !lineRef));
"|"          => ( Tokens.OR (!lineRef, !lineRef));
":="         => ( Tokens.ASSIGN (!lineRef, !lineRef));
"="          => ( Tokens.EQ (!lineRef, !lineRef));

{digits}      => ( Tokens.INT (toInt yytext, !lineRef, !lineRef));
{id}          => ( Tokens.ID (yytext, !lineRef, !lineRef));
.             => (err yypos ("Invalid !! '" ^ yytext ^ "' found"); lex());