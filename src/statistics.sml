(* use "utils.sml"; *)

structure Statistics  = struct
open Math;

exception ListasComTamanhosDiferentes

(* Retorna a soma de valores de uma lista *)
fun sum (head::tail) = head + sum(tail) | sum(nil)=0.0;

(* Retorna a quantidade de elementos em uma lista *)
fun length(head::tail) = 1.0 + length(tail) | length(nil) = 0.0;


fun mean(amostra) = sum(amostra) / length(amostra);

(* Somatório Simples *)
fun sigma_summation_s(head::tail, mean) = pow(head - mean, 2.0) + sigma_summation_s(tail, mean) | sigma_summation_s(nil, mean) = 0.0;

(* Somatório Duplo *)
fun sigma_summation_d(head_x::tail_x, head_y::tail_y, mean_x, mean_y) = (head_x - mean_x) * (head_y - mean_y) + sigma_summation_d(tail_x, tail_y, mean_x, mean_y)
                                                                        | sigma_summation_d(nil, nil, mean_x, mean_y) = 0.0
                                                                        | sigma_summation_d(nil, tail_y, mean_x, mean_y) = raise ListasComTamanhosDiferentes
                                                                        | sigma_summation_d(tail_x, nil, mean_x, mean_y) = raise ListasComTamanhosDiferentes;

fun correlation(amostra_x, amostra_y) = sigma_summation_d(amostra_x, amostra_y, mean(amostra_x), mean(amostra_y)) / sqrt(sigma_summation_s(amostra_x, mean(amostra_x)) * sigma_summation_s(amostra_y, mean(amostra_y)));

fun covariance(amostra_x, amostra_y) = sigma_summation_d(amostra_x, amostra_y, mean(amostra_x), mean(amostra_y)) / (length(amostra_x) - 1.0);

fun B(amostra_x, amostra_y) = (sigma_summation_d(amostra_x, amostra_y, mean(amostra_x), mean(amostra_y)) / length(amostra_x)) / (sigma_summation_s(amostra_x, mean(amostra_x)) / length(amostra_x));

fun A(amostra_x, amostra_y) = mean(amostra_y) - B(amostra_x, amostra_y) * mean(amostra_x);

fun linearRegression(amostra_x, amostra_y) = concat[Int.toString(Real.round(A(amostra_x, amostra_y))), " + ", Int.toString(Real.round(B(amostra_x, amostra_y))), "x"];

fun median(amostra) = List.nth(amostra, Real.round(length(amostra) / 2.0));

fun maximum(amostra: real list) =
 case amostra of [] => NONE
   | (head::[]) => SOME head
   | (head::neck::tail) =>	if head > neck
         then maximum(head::tail)
         else maximum(neck::tail);

fun minimum(amostra: real list) =
case amostra of [] => NONE
  | (head::[]) => SOME head
  | (head::neck::tail) =>	if head < neck
        then minimum(head::tail)
        else minimum(neck::tail);

(* Maximo menos minimo *)
fun range(amostra) = Option.valOf(maximum(amostra)) - Option.valOf(minimum(amostra));

(* standardDeviation *)
fun sigma_summation(head::tail, mean) = pow(head - mean, 2.0) + sigma_summation(tail, mean) | sigma_summation(nil, mean) = 0.0;
fun standardDeviation(amostra) = sqrt(1.0 / (length(amostra) - 1.0) * sigma_summation(amostra, mean(amostra)));

(* Variance *)
fun sigma_summation_pop(head::tail, mean, length) = pow(head - mean, 2.0) / length + sigma_summation_pop(tail, mean, length) | sigma_summation_pop(nil, mean, length) = 0.0;
fun variance_pop(amostra) = sigma_summation_pop(amostra, mean(amostra), length(amostra));

fun variance(amostra) = 1.0 / (length(amostra) - 1.0) * sigma_summation(amostra, mean(amostra));

end
