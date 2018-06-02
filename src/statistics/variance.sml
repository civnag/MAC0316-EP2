(*
 * Calcula a variância de uma amostra de flutuantes.
 * O retorno é um flutuante, resultado da amostra fornecida.
 *)

(* NOTA: Esta variância foi implementada com base nas duas fórmulas: para populações completas e para amostras.
 * A fórmula padrão será variance(amostra), que realizará o cálculo da variância de uma mostra.
 * A outra fórmula variance_pop é adicional e será incrementada a linguagem para populações.
*)

use "utils.sml";
use "mean.sml";
open Math;

fun sigma_summation_pop(head::tail, mean, length) = pow(head - mean, 2.0) / length + sigma_summation_p(tail, mean, length) | sigma_summation_p(nil, mean, length) = 0.0;
fun variance_pop(amostra) = sigma_summation_p(amostra, mean(amostra), length(amostra));

fun sigma_summation(head::tail, mean) = pow(head - mean, 2.0) + sigma_summation(tail, mean) | sigma_summation(nil, mean) = 0.0;
fun variance(amostra) = 1.0 / (length(amostra) - 1.0) * sigma_summation(amostra, mean(amostra));

