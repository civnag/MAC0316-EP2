
exception TypeMismatch;
exception OperationNotDefined;
exception DivisionByZero;
exception VariableNotDeclared;

fun updateHt(a: 'a,b: 'b,ht: ('a, 'b) HashTable.hash_table): unit = 
    let
        val achou = HashTable.find ht a
    in 
        case achou of
            NONE => raise VariableNotDeclared
            | SOME _ => 
                let 
                    val _ = HashTable.remove ht a 
                    val _ = HashTable.insert ht (a,b)
                in 
                    ()
                end
    end
            
structure Eval =
struct 

open Grammar;

(*
    Funcoes auxiliares para calculos e checagem de tipos.
*)

fun div_op((x,y): tipo *  tipo): tipo =
    case (x,y) of
        ( Primitivo ( Int_ i),  Primitivo( Int_ 0)) => 
            raise DivisionByZero
        | ( Primitivo ( Int_ i),  Primitivo( Int_ j)) => 
             Primitivo ( Int_ (Int.div(i,j))) 
        | ( Primitivo ( Float_ i),  Primitivo( Float_ j)) => 
                 Primitivo ( Float_ (i/j)) 
        | ( Primitivo n,  Primitivo q) => raise TypeMismatch
        | (_, _) => raise OperationNotDefined


fun mul_op((x,y): tipo *  tipo): tipo =
    case (x,y) of
        ( Primitivo ( Int_ i),  Primitivo( Int_ j)) => 
             Primitivo ( Int_ (i*j)) 
        | ( Primitivo ( Float_ i),  Primitivo( Float_ j)) => 
             Primitivo ( Float_ (i*j)) 
        | ( Primitivo n,  Primitivo q) => raise TypeMismatch
        | (_, _) => raise OperationNotDefined


fun sub_op((x,y): tipo *  tipo): tipo =
    case (x,y) of
        ( Primitivo ( Int_ i),  Primitivo( Int_ j)) => 
             Primitivo ( Int_ (i-j)) 
        | ( Primitivo ( Float_ i),  Primitivo( Float_ j)) => 
             Primitivo ( Float_ (i-j)) 
        | ( Primitivo n,  Primitivo q) => raise TypeMismatch
        | (_, _) => raise OperationNotDefined

fun sum_op((x,y): tipo *  tipo): tipo =
    case (x,y) of
        ( Primitivo ( Int_ i),  Primitivo( Int_ j)) => 
             Primitivo ( Int_ (i+j)) 
        | ( Primitivo ( Float_ i),  Primitivo( Float_ j)) => 
             Primitivo ( Float_ (i+j)) 
        | ( Primitivo ( String_ i),  Primitivo( String_ j)) => 
             Primitivo ( String_ (i^j)) 
        | ( Primitivo n,  Primitivo q) => raise TypeMismatch
        | (_, _) => raise OperationNotDefined

fun toString_op(x: tipo): tipo =
    case x of
         Primitivo ( Int_ v) => 
             Primitivo ( String_ (Int.toString v))
        |  Primitivo ( Float_ v) => 
             Primitivo ( String_ (Real.toString v))
        |  Primitivo ( String_ v) => 
             Primitivo ( String_ v)
        | _ => raise TypeMismatch

fun print_op(x: tipo): tipo =
    case x of
         Primitivo ( String_ v) => 
            let
                val _ = print v
            in
                 Primitivo  Void
            end
        | _ => raise TypeMismatch
(*
    Fim funcoes auxiliares
*)


(*
    eval: Funcao que calcula expressoes.
    Se a expressao for constante, retorna-se a apenas seu valor.
    Se for variavel, olhamos na tabela(memoria) e devolve este valor.
    Se for Add, Sub, Mul ou Div realiza-se a expressao aritmetica (tipos checados
    em sum_op, sub_op, ...)
    Se for ToString, convertemos um tipo primitivo para string (tipo checado em toString_op).
    Se for print, mostra a string na tela (tipo checado em print_op)
*)

fun eval (e:  Exp, m: Memory):  tipo =
    case e of
         Const n => n
        |  Variable var => 
            let 
                val achou = HashTable.find m var
            in
                case achou of
                    NONE => raise VariableNotDeclared
                    | SOME v => v
            end
        |  Add (e1,e2) => sum_op(eval(e1,m),eval(e2,m))
        |  Sub (e1,e2) => sub_op(eval(e1,m),eval(e2,m))
        |  Mul (e1,e2) => mul_op(eval(e1,m),eval(e2,m))
        |  Div (e1,e2) => div_op(eval(e1,m),eval(e2,m))
        |  ToString e => toString_op(eval(e,m))
        |  Print e => print_op(eval(e,m))



(* 
    exec: Funcao que interpreta o bloco. 
    Se for uma atribuicao, atualizamos a hashtable.
    Se for sequencia, executamos um comando e, recursivamente, executamos o proximo.
    Se for uma acao, apenas calculamos a expressao.
*)
fun exec(cmd:  Cmd, m: Memory):unit =
    case cmd of
          v := e => updateHt(v,eval(e,m),m)
         |  Seq (c :: cs) => 
            let 
                val _ = exec(c,m)
            in 
                exec( Seq cs,m)         
            end
        |  Seq Nil => ()
        |  Action e => 
            let 
                val _ = eval(e,m)
            in 
                ()
            end

(* run: Executa um programa. 
   Um programa eh composto por titulo, variaveis e o bloco (cmd).
   Uma hastable de 1000 posicoes sera criada e com isso, pode-se usar ateh 1000 vriaveis.
   Essa funcao cria as variaveis declaradas na tabela e executa o programa.
*)
fun run((title, vars, cmd)): unit = 
    let 
        val _ = print ("Programa " ^ title ^ "\n")
        val mem = HashTable.mkTable (HashString.hashString, op=) (1000, Fail "not found")
        val _ = List.app (fn(x) => HashTable.insert mem x) vars
    in 
        exec(cmd,mem)
    end

(* Teste da semantica. *)
fun pgmTeste():  Program = ("Primeiro programa"
                            , [("x", Primitivo ( Int_ 0)),("y", Primitivo ( Int_ 0))]
                            ,  Seq 
                                 ([ "x" := ( Add (( Variable "x"), ( Const ( Primitivo ( Int_ 1)))))
                                  , "y" := (( Mul (( Variable "x"), ( Const ( Primitivo ( Int_ 2))))))
                                  ,  Action ( Print ( ToString ( Variable "y")))
                                  ])
                            )

end