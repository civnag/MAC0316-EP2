(*
 * Calcula o desvio padrão de uma amostra de flutuantes.
 * O retorno é um flutuante, resultado da amostra fornecida.
 *)
use "utils.sml";
use "mean.sml";
open Math;

fun sigma_summation(head::tail, mean) = pow(head - mean, 2.0) + sigma_summation(tail, mean) | sigma_summation(nil, mean) = 0.0;
fun standardDeviation(amostra) = sqrt(1.0 / length(amostra) * sigma_summation(amostra, mean(amostra)));
