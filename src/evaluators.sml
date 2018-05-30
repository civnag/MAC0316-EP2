use "src/grammar.sml";

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

fun div_op((x,y):Grammar.tipo * Grammar.tipo):Grammar.tipo =
    case (x,y) of
        (Grammar.Primitivo (Grammar.Int_ i), Grammar.Primitivo(Grammar.Int_ 0)) => 
            raise DivisionByZero
        | (Grammar.Primitivo (Grammar.Int_ i), Grammar.Primitivo(Grammar.Int_ j)) => 
            Grammar.Primitivo (Grammar.Int_ (Int.div(i,j))) 
        | (Grammar.Primitivo (Grammar.Float_ i), Grammar.Primitivo(Grammar.Float_ j)) => 
                Grammar.Primitivo (Grammar.Float_ (i/j)) 
        | (Grammar.Primitivo n, Grammar.Primitivo q) => raise TypeMismatch
        | (_, _) => raise OperationNotDefined


fun mul_op((x,y):Grammar.tipo * Grammar.tipo):Grammar.tipo =
    case (x,y) of
        (Grammar.Primitivo (Grammar.Int_ i), Grammar.Primitivo(Grammar.Int_ j)) => 
            Grammar.Primitivo (Grammar.Int_ (i*j)) 
        | (Grammar.Primitivo (Grammar.Float_ i), Grammar.Primitivo(Grammar.Float_ j)) => 
            Grammar.Primitivo (Grammar.Float_ (i*j)) 
        | (Grammar.Primitivo n, Grammar.Primitivo q) => raise TypeMismatch
        | (_, _) => raise OperationNotDefined


fun sub_op((x,y):Grammar.tipo * Grammar.tipo):Grammar.tipo =
    case (x,y) of
        (Grammar.Primitivo (Grammar.Int_ i), Grammar.Primitivo(Grammar.Int_ j)) => 
            Grammar.Primitivo (Grammar.Int_ (i-j)) 
        | (Grammar.Primitivo (Grammar.Float_ i), Grammar.Primitivo(Grammar.Float_ j)) => 
            Grammar.Primitivo (Grammar.Float_ (i-j)) 
        | (Grammar.Primitivo n, Grammar.Primitivo q) => raise TypeMismatch
        | (_, _) => raise OperationNotDefined

fun sum_op((x,y):Grammar.tipo * Grammar.tipo):Grammar.tipo =
    case (x,y) of
        (Grammar.Primitivo (Grammar.Int_ i), Grammar.Primitivo(Grammar.Int_ j)) => 
            Grammar.Primitivo (Grammar.Int_ (i+j)) 
        | (Grammar.Primitivo (Grammar.Float_ i), Grammar.Primitivo(Grammar.Float_ j)) => 
            Grammar.Primitivo (Grammar.Float_ (i+j)) 
        | (Grammar.Primitivo (Grammar.String_ i), Grammar.Primitivo(Grammar.String_ j)) => 
            Grammar.Primitivo (Grammar.String_ (i^j)) 
        | (Grammar.Primitivo n, Grammar.Primitivo q) => raise TypeMismatch
        | (_, _) => raise OperationNotDefined

fun eval (e: Grammar.Exp, m:Grammar.Memory): Grammar.tipo =
    case e of
        Grammar.Const n => n
        | Grammar.Variable var => 
            let 
                val achou = HashTable.find m var
            in
                case achou of
                    NONE => raise VariableNotDeclared
                    | SOME v => v
            end
        | Grammar.Add (e1,e2) => sum_op(eval(e1,m),eval(e2,m))
        | Grammar.Sub (e1,e2) => sub_op(eval(e1,m),eval(e2,m))
        | Grammar.Mul (e1,e2) => mul_op(eval(e1,m),eval(e2,m))
        | Grammar.Div (e1,e2) => div_op(eval(e1,m),eval(e2,m))


fun exec(cmd: Grammar.Cmd, m:Grammar.Memory):unit =
    case cmd of
         Grammar.:= (v,e) => updateHt(v,eval(e,m),m)

end