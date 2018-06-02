(* Obtém o menor valor de uma amostra.
 * O retorno é um flutuante, resultado da amostra fornecida.
 *)

fun minimum(amostra: real list) =
 case amostra of [] => NONE
   | (head::[]) => SOME head
   | (head::neck::tail) =>	if head < neck
         then minimum(head::tail)
         else minimum(neck::tail);
