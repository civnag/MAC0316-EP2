structure Test =
struct

open Eval

fun main (prog_name, args) =
    let
      val _ = Eval.run(Eval.pgmTeste())
    in
      1
    end
end