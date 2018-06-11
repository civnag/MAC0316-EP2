structure Helper = struct
exception SizeNotAllowed
exception TypeError
open Grammar;

val isComma = Char.contains ","

fun deleteOpenParenthesis (list: char list) =
    case list of
      nil=> ""
      | xs::ys => if #"(" = xs then String.implode ys
          else Char.toString(xs) ^ deleteOpenParenthesis(ys)

fun deleteCloseParenthesis (list: char list) =
    case list of
      nil=> ""
      | xs::ys => if #")" = xs then String.implode ys
          else Char.toString(xs) ^ deleteCloseParenthesis(ys)

fun deleteOpenBracket (list: char list) =
    case list of
      nil=> ""
      | xs::ys => if #"{" = xs then String.implode ys
          else Char.toString(xs) ^ deleteOpenBracket(ys)

fun deleteCloseBracket (list: char list) =
    case list of
      nil=> ""
      | xs::ys => if #"}" = xs then String.implode ys
          else Char.toString(xs) ^ deleteCloseBracket(ys)

fun length nil = 0
    | length (x::xs) = 1 + length xs

fun removeParenthesis (tupleString: string) = deleteOpenParenthesis(String.explode(deleteCloseParenthesis(String.explode tupleString)));
fun removeBrackets (sampleString: string) = deleteOpenBracket(String.explode(deleteCloseBracket(String.explode sampleString)));

fun getCleanList(tupleString: string) =
  let
    val cleanString = removeParenthesis(tupleString)
  in
    String.tokens isComma cleanString
  end

fun getListFrom(sampleString: string) =
  let
    val cleanString = removeBrackets(sampleString)
  in
    List.map (fn (x)(t1) => auxToType x t1) String.tokens isComma cleanString
  end

fun auxToType x "string"  = Primitivo (String_ x)
  | auxToType x "int" = Primitivo (Int_ x)
  | auxToType x "boolean"  = Primitivo(Boolean_ x)
  | auxToType x "float" = Primitivo(Float_ x)
  | auxToType _ _ = raise TypeError

fun listToTupla(x1::x2::nil) (t1,t2) = Tupla2 (auxToType x1 t1, auxToType x2 t2)
  | listToTupla(x1::x2::x3::nil) (t1,t2,t3) = Tupla3 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3)
  | listToTupla(x1::x2::x3::x4::nil) (t1,t2,t3,t4) =
    Tupla4 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4)
  | listToTupla(x1::x2::x3::x4::x5::nil) (t1,t2,t3,t4,t5) =
    Tupla5 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5)
  | listToTupla(x1::x2::x3::x4::x5::x6::nil) (t1,t2,t3,t4,t5,t6) =
    Tupla6 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6)
  | listToTupla(x1::x2::x3::x4::x5::x6::x7::nil) (t1,t2,t3,t4,t5,t6,t7) =
    Tupla7 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6, auxToType x7 t7)
  | listToTupla(x1::x2::x3::x4::x5::x6::x7::x8::nil) (t1,t2,t3,t4,t5,t6,t7,t8) =
    Tupla8 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6, auxToType x7 t7, auxToType x8 t8)
  | listToTupla(x1::x2::x3::x4::x5::x6::x7::x8::x9::nil) (t1,t2,t3,t4,t5,t6,t7,t8,t9) =
    Tupla9 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6, auxToType x7 t7, auxToType x8 t8, auxToType x9 t9)
  | listToTupla(x1::x2::x3::x4::x5::x6::x7::x8::x9::x10::nil) (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) =
    Tupla0 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6, auxToType x7 t7, auxToType x8 t8, auxToType x9 t9, auxToType x10 t10)
  | listToTupla _ = raise SizeNotAllowed

fun getTupla (x: string) = listToTupla(getCleanList x)

fun getSample (x: string) = getListFrom x

fun isValueCorretType x "int" = Primitivo (Int_ intFromString(x))
  | isValueCorretType x "boolean"  = Primitivo(Boolean_ boolFromString(x))
  | isValueCorretType x "float" = Primitivo(Float_ floatFromString(x))
  | _ = Primitivo (String_ x)

fun intFromString s =
    case Int.fromString s of
         SOME i => i
       | NONE => raise Fail ("Could not get int value")

fun floatFromString s =
   case Real.fromString s of
        SOME i => i
      | NONE => raise Fail ("Could not get float vlaue")

fun boolFromString s =
   case Bool.fromString s of
        SOME i => i
      | NONE => raise Fail ("Could not get boolean value")

fun readFile filename =
    let val fd = TextIO.openIn filename
        val content = TextIO.inputAll fd handle e => (TextIO.closeIn fd; raise e)
        val _ = TextIO.closeIn fd
    in content end

fun writeFile filename content =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end

end
