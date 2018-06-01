(*
 * Calcula a mediana de uma amostra de flutuantes.
 * O retorno é um flutuante, resultado da amostra fornecida.
 *)
use "utils.sml";

fun median(amostra) = List.nth(amostra, Real.round(length(amostra) / 2.0));
