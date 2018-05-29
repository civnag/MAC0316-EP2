use "src/grammar.sml";

structure Eval =
struct 

fun eval (e: Grammar.Exp, m:Grammar.Memory): Grammar.tipo =
    case e of
        Grammar.Const n => n


end