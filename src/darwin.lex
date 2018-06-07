%name DarwinLexer;

%let digit = [0-9];
%let int = {digit}+;
%let alpha = [a-zA-Z];
%let id = {alpha}({alpha} | {digit})*;
%let str = ["]{id}["];
%let tipo = ("int"|"string"|"boolean");
%let bool = ("true"|"false")
%let float = {int}["."]({int}+|{digit}("e"|"E"){int});
%defs (
    structure T = DarwinTokens
    type lex_result = T.token
    fun eof() = T.EOF
);

let => ( T.KW_let );
"variables" => ( T.KW_variables );
"title" => ( T.KW_title );
"commands" => ( T.KW_comands );
"print" => ( T.KW_Print );
"end variables" => ( T.KW_endvars );
in => ( T.KW_in );
{tipo} => ( T.TIPO yytext );
{id} => ( T.ID yytext );
{str} => (T.STR yytext);
{int} => ( T.NUM (valOf (Int.fromString yytext)) );
{float} => ( T.REAL (valOf (Real.fromString yytext)) )
{bool} => ( T.BOOL (valOf (Bool.fromString yytext)) )
"=" => ( T.EQ );
";" => ( T.SEMI);
"+" => ( T.PLUS );
"-" => ( T.MINUS );
"*" => ( T.TIMES );
"/" => ( T.DIV );
"(" => ( T.LP );
")" => ( T.RP );
"." => ( T.DOT );
" " | \n | \t => ( continue() );
"terminate"   => ( T.KW_terminate ); 
.		=> (print (concat ["Unexpected character: '", yytext,
			           "'\n"]); continue());