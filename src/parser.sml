use "src/grammar.sml"; 

datatype 'a Parser = Parser of {parse : string -> ('a*string) list} 

exception StreamError;
exception ParsingError;

structure Parser =
struct 

fun runP(Parser{parse=p}, s: string): 'a = 
  let 
    val l = p s
  in
    case l of
      ((res, "") :: nil) => res
      | ((_, rs) :: nil)   => raise StreamError
      | _           => raise ParsingError
  end
  

fun item(): char Parser =
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