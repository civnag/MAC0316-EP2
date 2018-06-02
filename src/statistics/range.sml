(*
 * Calcula a distância de uma amostra de flutuantes.
 * O retorno é um flutuante, resultado da amostra fornecida.
 *)
use "maximum.sml";
use "minimum.sml";

fun range(amostra) = Option.valOf(maximum(amostra)) - Option.valOf(minimum(amostra));
