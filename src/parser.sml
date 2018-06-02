use "src/grammar.sml"; 


exception StreamError;
exception ParsingError;

datatype 'a Parser = Parser of {parse : string -> ('a*string) list} 

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

(* (Parser cs1) <*> (Parser cs2) = 
Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1]) *)

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


signature PARSER = 
sig 
  val runP : 'a Parser -> string -> 'a 
  val item : char Parser
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
end