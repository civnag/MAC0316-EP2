%name DarwinLexer;

%let digit = [0-9];
%let int = {digit}+;
%let alpha = [a-zA-Z];
%let id = {alpha}({alpha} | {digit})*;
%let str = "{id}";
%let tipo = [int,string,boolean];
%defs (
    structure T = DarwinTokens
    type lex_result = T.token
    fun eof() = T.EOF
);

let => ( T.KW_let );
"variables:" => ( T.KW_variables );
"title" => ( T.KW_title );
"comands:" => ( T.KW_comands );
"print" => ( T.KW_Print );
in => ( T.KW_in );
{tipo} => ( T.TIPO yytext );
{id} => ( T.ID yytext );
{str} => (T.STR yytext);
{int} => ( T.NUM (valOf (Int.fromString yytext)) );
"=" => ( T.EQ );
";" => ( T.SEMI);
"+" => ( T.PLUS );
"-" => ( T.MINUS );
"*" => ( T.TIMES );
"(" => ( T.LP );
")" => ( T.RP );
" " | \n | \t => ( continue() );
"end"   => ( eof() ); 
.		=> (print (concat ["Unexpected character: '", yytext,
			           "'\n"]); continue());