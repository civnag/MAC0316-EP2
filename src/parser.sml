signature PARSER = 
sig
  val runP : 'a Combinator.Parser -> string -> 'a 
  val item : char Combinator.Parser
  val satisfy : (char -> bool) -> char Combinator.Parser
  val oneOf : string -> char Combinator.Parser
  val auxchain : 'a Combinator.Parser -> ('a -> 'a -> 'a) Combinator.Parser -> 'a Combinator.Parser
  val rest : 'a -> ('a -> 'a -> 'a) Combinator.Parser -> 'a Combinator.Parser -> 'a Combinator.Parser
  val chainleft : 'a Combinator.Parser -> ('a -> 'a -> 'a) Combinator.Parser -> 'a -> 'a Combinator.Parser
end 


structure Helpers : PARSER =
struct 

open Combinator

infix 1 >>=
infix 4 <$>
infix 4 <*>
infix 4 <|>


exception StreamError;
exception ParsingError;

(****** Funcoes para imitar as expressoes regulares ********)


fun runP (Parser{parse=p}) s = 
  let 
    val l = p s
  in
    case l of
      ((res, "") :: nil) => res
      | ((_, rs) :: nil)   => raise StreamError
      | _           => raise ParsingError
  end
  
val item =
  Parser ({parse = fn(s: string) =>
    let 
      val css = String.explode s 
    in
      case css of
          nil => nil
          | (c::cs) => [(c,String.implode cs)]
    end
  }) 
  
fun satisfy pred = item >>= (fn(c) => 
  if (pred c) 
  then ret c 
  else pfail
)

fun oneOf s = satisfy (elem s)

fun rest a po p = (po >>= (fn(f) => p >>= (fn(b) => rest (f a b) po p))) <|> ret a  

fun auxchain p po = p >>= (fn(a) => rest a po p)

fun chainleft p po a = (auxchain p po) <|> ret a

end

