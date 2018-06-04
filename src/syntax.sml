signature SYNTAX  =
sig
  val charp : char -> char Combinator.Parser
  val nump : int Combinator.Parser
  val stringp : char list -> string Combinator.Parser
  val spaces : string Combinator.Parser
  val digit : char Combinator.Parser
  val tokenp : 'a Combinator.Parser -> 'a Combinator.Parser
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


end