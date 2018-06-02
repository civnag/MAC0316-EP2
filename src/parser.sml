use "src/grammar.sml"; 

datatype 'a Parser = Parser of {parse : string -> ('a*string) list} 

structure Parser =
struct 

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