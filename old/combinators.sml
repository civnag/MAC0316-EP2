signature COMB =
sig
    type 'a Parser
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

open SMLofNJ.Susp

type 'a Parser = string -> ('a*string) list

(* CURRYING *)

fun curry abc a b = abc (a,b);

(************)

(* Verifica se um carater pertence a uma string *)
fun elem s c = String.isSubstring (str c) s 

fun ret a = fn(s) => [(a,s)]

infix 1 >>=;

fun p >>= f = (fn(s) => 
    List.concat (List.map (fn(a, s') => 
          f a s'
    ) (p s))
)

infix 4 <$>;

fun f <$> p = (fn(s) =>
    let 
        val as' = p s 
    in 
        List.map (fn (a,s') => (f a, s')) as'
    end
)

infix 4 <*>;

fun cs <*> p = (fn(s) =>
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
)

infix 4 <|>;

fun p <|> f = (fn(s) =>
    let
        val ps = p s 
    in 
        case ps of
          nil => f s 
          | x => x
    end
)

fun pcombine p q = (fn(s) =>
    let
        val ps = p s 
        val qs = q s 
    in 
        ps @ qs
    end
)

val pfail = fn(s) => nil

fun some(p: 'a Parser): ('a list) Parser = some_v p
    and many_v p = 
        let 
            val aux = delay(fn () => (fn(x) => some_v p <|> x))
        in
            (force aux) (ret nil)
        end
    and some_v p = 
        let 
            val aux = delay (fn () => (many_v p))
        in
            ((curry (op ::)) <$> p) <*> force aux
        end
fun many(p: 'a Parser): ('a list) Parser = many_v p
    and many_v p = (some_v p) <|> ret nil
    and some_v p = ((curry (op ::)) <$> p) <*> (many_v p)


end