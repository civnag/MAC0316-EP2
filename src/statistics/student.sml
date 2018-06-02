(*
 * Realiza o teste t-student de UMA amostra de flutuantes de acordo com um valor fixo "mi".
 * O retorno é um flutuante, resultado da amostra fornecida.
 *)
use "utils.sml";
use "mean.sml";
use "standardDeviation.sml";
open Math;

fun student(amostra, mi) = (mean(amostra) - mi) / (standardDeviation(amostra) / sqrt(length(amostra)));
