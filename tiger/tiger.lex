type lineNo            = int
type pos               = lineNo  (* The type of Should match with expr.yacc *)
val  lineRef : pos ref = ref 0   (* reference variable to keep track of position.
                                            Typing not necessary just for clarity *)

fun updateLine n      = lineRef := !(lineRef) + n


type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token

fun lineRange l r = "line " ^ l
fun error (e,l,r) = TextIO.output(TextIO.stdErr, lineRange l r ^ ":" ^ e ^ "\n")



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

%header (functor ExprLexFun(structure Tokens : Expr_TOKENS));
ws    = [\ \t];
digits = [0-9]+;
id = [a-zA-Z_]+

%%

"#".*\n         => ( updateLine 1; lex ());
{ws}+           => ( lex() );
\n({ws}*\n)*    => (    let val old = !lineRef
                        in updateLine (newlineCount yytext); Tokens.NEWLINE (old, !lineRef)
                        end
                    );

"array"         => ( Tokens.ARRAY (yytext, !lineRef, !lineRef));
"if"            => ( Tokens.IF (yytext, !lineRef, !lineRef));
"then"          => ( Tokens.THEN (yytext, !lineRef, !lineRef));
"else"          => ( Tokens.ELSE (yytext, !lineRef, !lineRef));
"while"          => ( Tokens.WHILE (yytext, !lineRef, !lineRef));
"for"          => ( Tokens.FOR (yytext, !lineRef, !lineRef));
"to"          => ( Tokens.TO (yytext, !lineRef, !lineRef));
"do"          => ( Tokens.DO (yytext, !lineRef, !lineRef));
"let"          => ( Tokens.LET (yytext, !lineRef, !lineRef));
"in"          => ( Tokens.IN (yytext, !lineRef, !lineRef));
"end"          => ( Tokens.END (yytext, !lineRef, !lineRef));
"of"          => ( Tokens.OF (yytext, !lineRef, !lineRef));
"break"          => ( Tokens.BREAK (yytext, !lineRef, !lineRef));
"nil"          => ( Tokens.NIL (yytext, !lineRef, !lineRef));
"function"          => ( Tokens.FUNCTION (yytext, !lineRef, !lineRef));
"var"          => ( Tokens.VAR (yytext, !lineRef, !lineRef));
"type"          => ( Tokens.TYPE (yytext, !lineRef, !lineRef));
"import"          => ( Tokens.IMPORT (yytext, !lineRef, !lineRef));
"primitive"          => ( Tokens.PRIMITIVE (yytext, !lineRef, !lineRef));


"("          => ( Tokens.LPARAN (yytext, !lineRef, !lineRef));
")"          => ( Tokens.RPARAN (yytext, !lineRef, !lineRef));
"["          => ( Tokens.LBRACK (yytext, !lineRef, !lineRef));
"]"          => ( Tokens.RBRACK (yytext, !lineRef, !lineRef));
"{"          => ( Tokens.LCURLY (yytext, !lineRef, !lineRef));
"}"          => ( Tokens.RCURLY (yytext, !lineRef, !lineRef));
"."          => ( Tokens.DOT (yytext, !lineRef, !lineRef));
"+"          => ( Tokens.PLUS (yytext, !lineRef, !lineRef));
"-"          => ( Tokens.MINUS (yytext, !lineRef, !lineRef));
"*"          => ( Tokens.MUL (yytext, !lineRef, !lineRef));
"/"          => ( Tokens.DIV (yytext, !lineRef, !lineRef));
"="          => ( Tokens.EQ (yytext, !lineRef, !lineRef));
"<>"          => ( Tokens.NEQ (yytext, !lineRef, !lineRef));
"<"          => ( Tokens.LT (yytext, !lineRef, !lineRef));
"<="          => ( Tokens.LTE (yytext, !lineRef, !lineRef));
">"          => ( Tokens.GT (yytext, !lineRef, !lineRef));
">="          => ( Tokens.GTE (yytext, !lineRef, !lineRef));
"&"          => ( Tokens.AND (yytext, !lineRef, !lineRef));
"|"          => ( Tokens.OR (yytext, !lineRef, !lineRef));
":="          => ( Tokens.ASSIGN (yytext, !lineRef, !lineRef));

{digits}        => ( Tokens.INT (toInt yytext, !lineRef, !lineRef));
{id}            => (Token.ID (yytext, !lineRef, !lineRef));
