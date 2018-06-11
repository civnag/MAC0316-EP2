structure Help = struct 
exception SizeNotAllowed
exception TypeError

val isComma = Char.contains ","

fun deleteOpenParenthesis (list: char list) =
    case list of
      []=> ""
      | xs::ys => if #"(" = xs then String.implode ys
          else Char.toString(xs) ^ deleteOpenParenthesis(ys)

fun deleteCloseParenthesis (list: char list) =
    case list of
      []=> ""
      | xs::ys => if #")" = xs then String.implode ys
          else Char.toString(xs) ^ deleteCloseParenthesis(ys)

fun length [] = 0
    | length (x::xs) = 1 + length xs

fun removeParenthesis (tupleString: string) = deleteOpenParenthesis(String.explode(deleteCloseParenthesis(String.explode tupleString)));

fun getCleanList(tupleString: string) =
  let
    val cleanString = removeParenthesis(tupleString)
  in
    String.tokens isComma cleanString
  end

open Grammar;

fun auxToType "string" x = Primitivo (String_ x)
  | auxToType "int" x = Primitivo (Int_ x)
  | auxToType "boolean" x = Primitivo(Boolean_ x)
  | auxToType "float" x = Primitivo(Float_ x)
  | auxToType _ _ = raise TypeError

fun listToTupla(x1::x2::[]) (t1,t2) = Tupla2 (auxToType x1 t1, auxToType x2 t2)
  | listToTupla(x1::x2::x3::[]) (t1,t2,t3) = Tupla3 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3)
  | listToTupla(x1::x2::x3::x4::[]) (t1,t2,t3,t4) = 
    Tupla4 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4)
  | listToTupla(x1::x2::x3::x4::x5::[]) (t1,t2,t3,t4,t5) = 
    Tupla5 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5)
  | listToTupla(x1::x2::x3::x4::x5::x6::[]) (t1,t2,t3,t4,t5,t6) = 
    Tupla6 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6)
  | listToTupla(x1::x2::x3::x4::x5::x6::x7::[]) (t1,t2,t3,t4,t5,t6,t7) = 
    Tupla7 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6, auxToType x7 t7)
  | listToTupla(x1::x2::x3::x4::x5::x6::x7::x8::[]) (t1,t2,t3,t4,t5,t6,t7,t8) = 
    Tupla8 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6, auxToType x7 t7, auxToType x8 t8)
  | listToTupla(x1::x2::x3::x4::x5::x6::x7::x8::x9::[]) (t1,t2,t3,t4,t5,t6,t7,t8,t9) = 
    Tupla9 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6, auxToType x7 t7, auxToType x8 t8, auxToType x9 t9)
  | listToTupla(x1::x2::x3::x4::x5::x6::x7::x8::x9::x10::[]) (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) = 
    Tupla0 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6, auxToType x7 t7, auxToType x8 t8, auxToType x9 t9, auxToType x10 t10)
  | listToTupla _ = raise SizeNotAllowed

fun getTupla (x: string) = listToTupla(getCleanList x)

fun getSample (x: string) = getCleanList x

end