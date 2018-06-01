(*
 * Calcula a média aritmética de uma amostra de flutuantes.
 * O retorno é um flutuante, resultado da amostra fornecida.
 *)
use "utils.sml";

fun mean(amostra) = sum(amostra) / length(amostra);
