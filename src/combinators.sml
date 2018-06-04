signature COMB =
sig
    datatype 'a Parser = Parser of {parse : string -> ('a*string) list} 
    val curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c
    val elem : string -> char -> bool
    val >>= : 'a Parser * ('a -> 'b Parser) -> 'b Parser
    val <*> : ('a -> 'b) Parser * 'a Parser -> 'b Parser
    val <$> : ('a -> 'b) * 'a Parser -> 'b Parser
    val <|> : 'a Parser * 'a Parser -> 'a Parser
    val ret : 'a -> 'a Parser
    val some : 'a Parser -> ('a list) Parser
    val many : 'a Parser -> ('a list) Parser
    val pfail : 'a Parser
    val pcombine : 'a Parser -> 'a Parser -> 'a Parser
end

structure Combinator : COMB = 
struct

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

end