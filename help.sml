exception SizeNotAllowed

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

fun listToTupla(x1::x2::[]) = Tupla2 (x1, x2)
  | listToTupla(x1::x2::x3::[]) = Tupla3 (x1, x2, x3)
  | listToTupla(x1::x2::x3::x4::[]) = Tupla4 (x1, x2, x3, x4)
  | listToTupla(x1::x2::x3::x4::x5::[]) = Tupla5 (x1, x2, x3, x4, x5)
  | listToTupla(x1::x2::x3::x4::x5::x6::[]) = Tupla6 (x1, x2, x3, x4, x5, x6)
  | listToTupla(x1::x2::x3::x4::x5::x6::x7::[]) = Tupla7 (x1, x2, x3, x4, x5, x6, x7)
  | listToTupla(x1::x2::x3::x4::x5::x6::x7::x8::[]) = Tupla8 (x1, x2, x3, x4, x5, x6, x7, x8)
  | listToTupla(x1::x2::x3::x4::x5::x6::x7::x8::x9::[]) = Tupla9 (x1, x2, x3, x4, x5, x6, x7, x8, x9)
  | listToTupla(x1::x2::x3::x4::x5::x6::x7::x8::x9::x10::[]) = Tupla0 (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  | listToTupla _ = raise SizeNotAllowed

fun getTupla (x: string) = listToTupla(getCleanList x)

fun getSample (x: string) = getCleanList x
