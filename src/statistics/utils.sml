(* Retorna a soma de valores de uma lista *)
fun sum (head::tail) = head + sum(tail) | sum(nil)=0.0;

(* Retorna a quantidade de elementos em uma lista *)
fun length(head::tail) = 1.0 + length(tail) | length(nil) = 0.0;
