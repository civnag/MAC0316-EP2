use "src/grammar.sml";

exception TypeMismatch;
exception OperationNotDefined;
exception DivisionByZero;
exception VariableNotDeclared;

structure G = Grammar

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

(*
    Funcoes auxiliares para calculos e checagem de tipos.
*)

fun div_op((x,y):G.tipo * G.tipo):G.tipo =
    case (x,y) of
        (G.Primitivo (G.Int_ i), G.Primitivo(G.Int_ 0)) => 
            raise DivisionByZero
        | (G.Primitivo (G.Int_ i), G.Primitivo(G.Int_ j)) => 
            G.Primitivo (G.Int_ (Int.div(i,j))) 
        | (G.Primitivo (G.Float_ i), G.Primitivo(G.Float_ j)) => 
                G.Primitivo (G.Float_ (i/j)) 
        | (G.Primitivo n, G.Primitivo q) => raise TypeMismatch
        | (_, _) => raise OperationNotDefined


fun mul_op((x,y):G.tipo * G.tipo):G.tipo =
    case (x,y) of
        (G.Primitivo (G.Int_ i), G.Primitivo(G.Int_ j)) => 
            G.Primitivo (G.Int_ (i*j)) 
        | (G.Primitivo (G.Float_ i), G.Primitivo(G.Float_ j)) => 
            G.Primitivo (G.Float_ (i*j)) 
        | (G.Primitivo n, G.Primitivo q) => raise TypeMismatch
        | (_, _) => raise OperationNotDefined


fun sub_op((x,y):G.tipo * G.tipo):G.tipo =
    case (x,y) of
        (G.Primitivo (G.Int_ i), G.Primitivo(G.Int_ j)) => 
            G.Primitivo (G.Int_ (i-j)) 
        | (G.Primitivo (G.Float_ i), G.Primitivo(G.Float_ j)) => 
            G.Primitivo (G.Float_ (i-j)) 
        | (G.Primitivo n, G.Primitivo q) => raise TypeMismatch
        | (_, _) => raise OperationNotDefined

fun sum_op((x,y):G.tipo * G.tipo):G.tipo =
    case (x,y) of
        (G.Primitivo (G.Int_ i), G.Primitivo(G.Int_ j)) => 
            G.Primitivo (G.Int_ (i+j)) 
        | (G.Primitivo (G.Float_ i), G.Primitivo(G.Float_ j)) => 
            G.Primitivo (G.Float_ (i+j)) 
        | (G.Primitivo (G.String_ i), G.Primitivo(G.String_ j)) => 
            G.Primitivo (G.String_ (i^j)) 
        | (G.Primitivo n, G.Primitivo q) => raise TypeMismatch
        | (_, _) => raise OperationNotDefined

fun toString_op(x:G.tipo):G.tipo =
    case x of
        G.Primitivo (G.Int_ v) => 
            G.Primitivo (G.String_ (Int.toString v))
        | G.Primitivo (G.Float_ v) => 
            G.Primitivo (G.String_ (Real.toString v))
        | G.Primitivo (G.String_ v) => 
            G.Primitivo (G.String_ v)
        | _ => raise TypeMismatch

fun print_op(x:G.tipo):G.tipo =
    case x of
        G.Primitivo (G.String_ v) => 
            let
                val _ = print v
            in
                G.Primitivo G.Void
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

fun eval (e: G.Exp, m:G.Memory): G.tipo =
    case e of
        G.Const n => n
        | G.Variable var => 
            let 
                val achou = HashTable.find m var
            in
                case achou of
                    NONE => raise VariableNotDeclared
                    | SOME v => v
            end
        | G.Add (e1,e2) => sum_op(eval(e1,m),eval(e2,m))
        | G.Sub (e1,e2) => sub_op(eval(e1,m),eval(e2,m))
        | G.Mul (e1,e2) => mul_op(eval(e1,m),eval(e2,m))
        | G.Div (e1,e2) => div_op(eval(e1,m),eval(e2,m))
        | G.ToString e => toString_op(eval(e,m))
        | G.Print e => print_op(eval(e,m))



(* 
    exec: Funcao que interpreta o bloco. 
    Se for uma atribuicao, atualizamos a hashtable.
    Se for sequencia, executamos um comando e, recursivamente, executamos o proximo.
    Se for uma acao, apenas calculamos a expressao.
*)
fun exec(cmd: G.Cmd, m:G.Memory):unit =
    case cmd of
         G.:= (v, e) => updateHt(v,eval(e,m),m)
         | G.Seq (c :: cs) => 
            let 
                val _ = exec(c,m)
            in 
                exec(G.Seq cs,m)         
            end
        | G.Seq Nil => ()
        | G.Action e => 
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
fun pgmTeste(): G.Program = ("Primeiro programa"
                            , [("x",G.Primitivo (G.Int_ 0)),("y",G.Primitivo (G.Int_ 0))]
                            , G.Seq 
                                 ([G.:= ("x", (G.Add ((G.Variable "x"), (G.Const (G.Primitivo (G.Int_ 1))))))
                                  , G.:= ("y", ((G.Mul ((G.Variable "x"), (G.Const (G.Primitivo (G.Int_ 2)))))))
                                  , G.Action (G.Print (G.ToString (G.Variable "y")))
                                  ])
                            )

end