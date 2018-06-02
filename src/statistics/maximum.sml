(* Obtém o maior valor de uma amostra.
 * O retorno é um flutuante, resultado da amostra fornecida.
 *)

fun maximum(amostra: real list) =
 case amostra of [] => NONE
   | (head::[]) => SOME head
   | (head::neck::tail) =>	if head > neck
         then maximum(head::tail)
         else maximum(neck::tail);
