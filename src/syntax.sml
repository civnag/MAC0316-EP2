signature SYNTAX  =
sig
  val charp : char -> char Combinator.Parser
  val nump : int Combinator.Parser
  val stringp : char list -> string Combinator.Parser
  val spaces : string Combinator.Parser
  val digit : char Combinator.Parser
  val tokenp : 'a Combinator.Parser -> 'a Combinator.Parser
  val reserved : string -> string Combinator.Parser
  val isnumberp : int Combinator.Parser
  val parens : 'a Combinator.Parser -> 'a Combinator.Parser
  val infixOp : string -> ('a * 'a -> 'a) -> ('a * 'a -> 'a) Combinator.Parser
  
  val intp :  Grammar.Exp Combinator.Parser 
  val expressionp : unit ->  Grammar.Exp Combinator.Parser 
  (* val factor :  Exp Parser *)
  (* val termp : unit ->  Exp Parser *)
  val addop : ( Grammar.Exp *  Grammar.Exp ->  Grammar.Exp) Combinator.Parser
  val mulop : ( Grammar.Exp *  Grammar.Exp ->  Grammar.Exp) Combinator.Parser
  
  val runLine : string ->  Grammar.Exp 
end


(****** PARSERS DE FATO ********)

structure Syntax : SYNTAX =
struct
open Grammar
open Combinator

infix 1 >>=
infix 4 <$>
infix 4 <*>
infix 4 <|>

open Helpers

exception StreamError;
exception ParsingError;


(* Satifaz um dado char *)
fun charp c = satisfy (fn(x) => c = x)

(* Eh numero? *)
val nump = Option.valOf <$> (Int.fromString <$> (String.implode <$> some (satisfy Char.isDigit)))

fun stringp nil = ret ""
  | stringp (c::cs) = (charp c) >>= (fn(x) => stringp cs >>= (fn(y) => ret (String.implode(c::cs))))

val spaces = String.implode <$> (many (oneOf " \n\r"))

val digit = satisfy (Char.isDigit)

fun tokenp p = p >>= (fn(a) => spaces >>= (fn(x) => ret a))

fun reserved s = tokenp (stringp (String.explode s))

val isnumberp = (stringp (String.explode "-") <|> ret "") >>= (fn(s) => String.implode <$> (some digit) >>= (fn(cs) => ret (Option.valOf (Int.fromString (s ^ cs)))))

fun parens p = reserved "(" >>= (fn(y) => p >>= (fn(n) => reserved ")" >>= (fn(x) => ret n)))

fun infixOp s f = reserved s >>= (fn(x) => ret f)

(****** Conversao para a AST *******)

val intp = isnumberp >>= (fn(n) => ret ( Const ( Primitivo ( Int_ n)))) 

val addop = (infixOp "+"  Add) <|> (infixOp "-"  Sub)

val mulop = (infixOp "*"  Mul) <|> (infixOp "/"  Div) 

fun expressionp() = auxchain (auxchain (intp <|> parens (expressionp())) (curry <$> mulop)) (curry <$> addop)

val runLine = runP (expressionp()) 

end