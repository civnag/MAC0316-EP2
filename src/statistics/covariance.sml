(*
 * Calcula a covariância de duas amostras de flutuantes.
 * O retorno é um flutuante, resultado das amostras fornecidas.
 *)
 use "utils.sml";
 use "mean.sml";
exception ListasComTamanhosDiferentes;

 fun sigma_summation_d(head_x::tail_x, head_y::tail_y, mean_x, mean_y) = (head_x - mean_x) * (head_y - mean_y) + sigma_summation_d(tail_x, tail_y, mean_x, mean_y)
                                                                         | sigma_summation_d(nil, nil, mean_x, mean_y) = 0.0
                                                                         | sigma_summation_d(nil, tail_y, mean_x, mean_y) = raise ListasComTamanhosDiferentes
                                                                         | sigma_summation_d(tail_x, nil, mean_x, mean_y) = raise ListasComTamanhosDiferentes;

fun covariance(amostra_x, amostra_y) = sigma_summation_d(amostra_x, amostra_y, mean(amostra_x), mean(amostra_y)) / (length(amostra_x) - 1.0);
