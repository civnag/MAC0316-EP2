%name DarwinLexer;

%let digit = [0-9];
%let int = {digit}+;
%let alpha = [a-zA-Z];
%let id = {alpha}({alpha} | {digit})*;
%let str = ["]{id}["];
%let primitivo = ("int"|"string"|"boolean"|"float");
%let tuple = "tuple" "(" ({primitivo} ("," {primitivo}){1,9} ")" );
%let lista = ("sample of "({primitivo}|{tuple}));  
%let tipo = ({primitivo}|{tuple}|{lista});
%let true = "true";
%let false = "false";
%let boolean = ({true}|{false});
%let float = {int}["."]({int}+|{digit}("e"|"E"){int});
%let valPrim = ({int} | {str} | {boolean} | {float});
%let tupleVal = ( "(" {valPrim} ("," {valPrim}){1,9} ")" );
%let empty = "{}";
%let intList = ({empty} | "{" {int} ("," {int})* "}" );
%let floatList = ({empty} | "{" {float} ("," {float})* "}" );
%let booleanList = ({empty} | "{" {boolean} ("," {boolean})* "}" );
%let strList = ({empty} | "{" {str} ("," {str})* "}" );
%let tupleList = ({empty} | "{" {tupleVal} ("," {tupleVal})* "}" );
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
"sum" => ( T.KW_SUM );
"prod" => ( T.KW_PROD );
"end variables" => ( T.KW_endvars );
in => ( T.KW_in );
{tipo} => ( print yytext; T.TIPO yytext );
{id} => ( T.ID yytext );
{str} => (T.STR yytext);
{int} => ( T.NUM (valOf (Int.fromString yytext)) );
{float} => ( T.REAL (valOf (Real.fromString yytext)) );
{boolean} => ( T.BOOL (valOf (Bool.fromString yytext)) );
{tupleVal} => (T.STR yytext);
{intList} => (print yytext; T.SINT (Grammar.toIntList (Grammar.tokenize yytext)));
{floatList} => (print yytext; T.SFLOAT (Grammar.toFloatList (Grammar.tokenize yytext)));
{booleanList} => (print yytext; T.SBOOL (Grammar.toBoolList (Grammar.tokenize yytext)));
{strList} => (print yytext; T.SSTRING (Grammar.tokenize yytext));
"=" => ( T.EQ );
"==" => ( T.EEQ );
";" => ( T.SEMI);
"+" => ( T.PLUS );
"-" => ( T.MINUS );
"*" => ( T.TIMES );
"/" => ( T.DIV );
"(" => ( T.LP );
")" => ( T.RP );
"." => ( T.DOT );
"&&" => ( T.AND );
"||" => ( T.OR );
"!" => ( T.NOT );
">=" => ( T.GEQ );
"<=" => ( T.LEQ );
">=" => ( T.GT );
"<=" => ( T.LT );
"!=" => ( T.NEQ );
"{}" => ( T.EMPTY );
"," => ( T.COMMA );
" " | \n | \t => ( continue() );
"terminate"   => ( T.KW_terminate ); 
.		=> (print (concat ["Unexpected character: '", yytext,
			           "'\n"]); continue());