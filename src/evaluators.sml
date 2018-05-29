use "src/grammar.sml";

structure Eval =
struct 

fun sum_op((x,y):Grammar.tipo * Grammar.tipo):Grammar.tipo =
    case (x,y) of
        (Grammar.Primitivo (Grammar.Int_ i), Grammar.Primitivo(Grammar.Int_ j)) => 
            Grammar.Primitivo (Grammar.Int_ (i+j)) 

fun eval (e: Grammar.Exp, m:Grammar.Memory): Grammar.tipo =
    case e of
        Grammar.Const n => n


end