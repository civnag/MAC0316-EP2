use "src/grammar.sml"; 
use "src/evaluators.sml"; 

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
  val charp : char -> char Parser
  val nump : int Parser
  val stringp : char list -> string Parser
  val spaces : string Parser
  val digit : char Parser
  val tokenp : 'a Parser -> 'a Parser
  val reserved : string -> string Parser
  val isnumberp : int Parser
  val parens : 'a Parser -> 'a Parser
  val infixOp : string -> ('a * 'a -> 'a) -> ('a * 'a -> 'a) Parser
  
  val intp : G.Exp Parser 
  val expressionp : unit -> G.Exp Parser 
  (* val factor : G.Exp Parser *)
  (* val termp : unit -> G.Exp Parser *)
  val addop : (G.Exp * G.Exp -> G.Exp) Parser
  val mulop : (G.Exp * G.Exp -> G.Exp) Parser
  
  val runLine : string -> G.Exp 
end 


structure Syntax : PARSER =
struct 

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

(****** PARSERS DE FATO ********)

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

val intp = isnumberp >>= (fn(n) => ret (G.Const (G.Primitivo (G.Int_ n)))) 

val addop = (infixOp "+" G.Add) <|> (infixOp "-" G.Sub)

val mulop = (infixOp "*" G.Mul) <|> (infixOp "/" G.Div) 

fun expressionp() = auxchain (auxchain (intp <|> parens (expressionp())) (curry <$> mulop)) (curry <$> addop)

val runLine = runP (expressionp()) 

end

