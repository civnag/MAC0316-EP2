use "grammar/grammar.sml";
structure Test =
struct

fun main (prog_name, args) =
    let
      val _ = print ("OLA MUNDO! " ^ Int.toString(Aux.algo(2)) ^  " \n")
    in
      1
    end
end