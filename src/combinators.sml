signature COMB =
sig
    type 'a Parser = {parse : string -> ('a*string) list} 
    val curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c
    val elem : string -> char -> bool
    val >>= : 'a Parser * ('a -> 'b Parser) -> 'b Parser
    val <*> : ('a -> 'b) Parser * 'a Parser -> 'b Parser
    val <$> : ('a -> 'b) * 'a Parser -> 'b Parser
    val <|> : 'a Parser * (unit -> 'a Parser) -> 'a Parser
    val ret : 'a -> 'a Parser
    val some : 'a Parser -> ('a list) Parser
    val many : 'a Parser -> ('a list) Parser
    val pfail : 'a Parser
    val pcombine : 'a Parser -> 'a Parser -> 'a Parser
end

structure Combinator : COMB = 
struct

type 'a Parser = {parse : string -> ('a*string) list} 

(* CURRYING *)

fun curry abc a b = abc (a,b);

(************)

(* Verifica se um carater pertence a uma string *)
fun elem s c = String.isSubstring (str c) s 

fun ret a = ({parse=fn(s) => [(a,s)]})

infix 1 >>=;

fun {parse=p} >>= f = ({parse=fn(s) => 
    List.concat (List.map (fn(a, s') => 
      let 
          val {parse=q} = f a 
      in
          q s'
      end) (p s))
})

infix 4 <$>;

fun f <$> {parse=p} = ({parse=fn(s) =>
    let 
        val as' = p s 
    in 
        List.map (fn (a,s') => (f a, s')) as'
    end
})

infix 4 <*>;

fun {parse=cs} <*> {parse=p} = ({parse=fn(s) =>
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

fun {parse=p} <|> f = ({parse=fn(s) =>
    let
        val ps = p s 
    in 
        case ps of
          nil => 
            let 
                val {parser=q} = f () 
            in 
                q s 
            end
          | x => x
    end
})

fun pcombine {parse=p} {parse=q} = ({parse=fn(s) =>
    let
        val ps = p s 
        val qs = q s 
    in 
        ps @ qs
    end
})

val pfail ={parse= fn(s) => nil}

open Lazy;

fun some(p: 'a Parser): ('a list) Parser = some_v p
    and many_v p = (some_v p) <|> (fn() => ret nil)
    and some_v p = ((curry (op ::)) <$> p) <*> (many_v p)
    
fun many(p: 'a Parser): ('a list) Parser = many_v p
    and many_v p = (some_v p) <|> (fn() => ret nil)
    and some_v p = ((curry (op ::)) <$> p) <*> (many_v p)


end