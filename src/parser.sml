use "src/grammar.sml"; 


exception StreamError;
exception ParsingError;

datatype 'a Parser = Parser of {parse : string -> ('a*string) list} 

(* CURRYING *)

fun curry abc a b = abc (a,b);

(************)

(* Verifica se um carater pertence a uma string *)
fun elem s c = String.isSubstring (str c) s 

fun ret a = Parser ({parse=fn(s) => [(a,s)]})

infix 1 >>=;

fun (Parser{parse=p}) >>= f = Parser ({parse=fn(s) => 
    List.concat (List.map (fn(a, s') => 
      let 
          val (Parser{parse=q}) = f a 
      in
          q s'
      end) (p s))
})

infix 4 <$>;

fun f <$> (Parser{parse=p}) = Parser ({parse=fn(s) =>
    let 
        val as' = p s 
    in 
        List.map (fn (a,s') => (f a, s')) as'
    end
})

infix 4 <*>;

fun (Parser{parse=cs}) <*> (Parser{parse=p}) = Parser ({parse=fn(s) =>
    let 
        val fs = cs s 
    in 
        List.concat(List.map (fn (f,s1) => 
          let 
            val as' = p s1 
          in 
            List.map (fn(a,s2) => (f a,s2)) as'
          end) fs)  
    end
})

infix 4 <|>;

fun (Parser{parse=p}) <|> (Parser{parse=q}) = Parser ({parse=fn(s) =>
    let
        val ps = p s 
    in 
        case ps of
          nil => q s
          | x => x
    end
})

fun pcombine (Parser{parse=p}) (Parser{parse=q}) = Parser ({parse=fn(s) =>
    let
        val ps = p s 
        val qs = q s 
    in 
        ps @ qs
    end
})

val pfail = Parser {parse= fn(s) => nil}

fun some(p: 'a Parser): ('a list) Parser = curry (op ::) <$> p <*> (some p <|> ret nil) 

fun many(p: 'a Parser): ('a list) Parser = (curry (op ::) <$> p <*> many p) <|> ret nil 

signature PARSER = 
sig 
  val runP : 'a Parser -> string -> 'a 
  val item : char Parser
  val satisfy : (char -> bool) -> char Parser
  val oneOf : string -> char Parser
  val auxchain : 'a Parser -> ('a -> 'a -> 'a) Parser -> 'a Parser
  val chainleft : 'a Parser -> ('a -> 'a -> 'a) Parser -> 'a -> 'a Parser
end 


structure Syntax : PARSER =
struct 

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

