structure Helper = struct
exception SizeNotAllowed
exception TypeError


open Grammar;

val isComma = Char.contains ","
val isSemi = Char.contains ";"


fun readFromString(s) = 
  case (Real.fromString s) of
      SOME n => Primitivo (Float_ n)
    | NONE => readInt(s)
and readInt(s) =
  case (Int.fromString s) of
      SOME f => Primitivo (Int_ f)
    | NONE => readBool(s)
and readBool(s) =
  case (Bool.fromString s) of
      SOME b => Primitivo (Boolean_ b)
    | NONE => Primitivo (String_ s)

fun toSemi nil = nil
  | toSemi((#")")::(#",")::ss) = (#")")::(#";")::(toSemi ss)
  | toSemi(x::xs) = x :: (toSemi xs)

fun toTupla(x1::x2::nil) = Tupla2 (readFromString x1, readFromString x2)
  | toTupla(x1::x2::x3::nil) = Tupla3 (readFromString x1, readFromString x2, readFromString x3)
  | toTupla(x1::x2::x3::x4::nil) = Tupla4 (readFromString x1, readFromString x2, readFromString x3,readFromString x4)
  | toTupla(x1::x2::x3::x4::x5::nil) = Tupla5 (readFromString x1, readFromString x2, readFromString x3,readFromString x4, readFromString x5)
  | toTupla(x1::x2::x3::x4::x5::x6::nil) = Tupla6 (readFromString x1, readFromString x2, readFromString x3,readFromString x4, readFromString x5, readFromString x6)
  | toTupla(x1::x2::x3::x4::x5::x6::x7::nil) = Tupla7 (readFromString x1, readFromString x2, readFromString x3,readFromString x4, readFromString x5, readFromString x6,readFromString x7)
  | toTupla(x1::x2::x3::x4::x5::x6::x7::x8::nil) = Tupla8 (readFromString x1, readFromString x2, readFromString x3,readFromString x4, readFromString x5, readFromString x6,readFromString x7, readFromString x8)
  | toTupla(x1::x2::x3::x4::x5::x6::x7::x8::x9::nil) = Tupla9 (readFromString x1, readFromString x2, readFromString x3,readFromString x4, readFromString x5, readFromString x6,readFromString x7, readFromString x8, readFromString x9)
  | toTupla(x1::x2::x3::x4::x5::x6::x7::x8::x9::x0::nil) = Tupla0 (readFromString x1, readFromString x2, readFromString x3,readFromString x4, readFromString x5, readFromString x6,readFromString x7, readFromString x8, readFromString x9,readFromString x0)
  | toTupla _ = raise SizeNotAllowed
    
fun removeHashTag(ls: string) = String.implode(List.filter (fn(l)=> not (l = #"#")) (String.explode(ls)))

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

fun removeParenthesis (tupleString: string) = removeHashTag(deleteOpenParenthesis(String.explode(deleteCloseParenthesis(String.explode tupleString))));
fun removeBrackets (sampleString: string) = deleteOpenBracket(String.explode(deleteCloseBracket(String.explode sampleString)));

fun getCleanList(tupleString) =
  let
    val cleanString = removeParenthesis(tupleString)
  in
    String.tokens isComma cleanString
  end

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

fun auxToType x "string"  = Primitivo (String_ x)
  | auxToType x "int" = Primitivo (Int_ (intFromString x))
  | auxToType x "boolean"  = Primitivo(Boolean_ (boolFromString x))
  | auxToType x "float" = Primitivo(Float_ (floatFromString x))
  | auxToType _ _ = raise TypeError

fun getListFrom(sampleString: string) =
  let
    val cleanString = removeBrackets(sampleString)
  in
    (String.tokens isComma cleanString)
  end

fun getTupleFrom(sampleString: string) =
  let
    val cleanString = removeParenthesis(sampleString)
  in
    (String.tokens isComma cleanString)
  end

fun listToTupla((x1::x2::nil), (t1::t2::nil)) = Tupla2 (auxToType x1 t1, auxToType x2 t2)
  | listToTupla((x1::x2::x3::nil), (t1::t2::t3::nil)) = Tupla3 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3)
  | listToTupla((x1::x2::x3::x4::nil), (t1::t2::t3::t4::nil)) = Tupla4 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4)
  | listToTupla((x1::x2::x3::x4::x5::nil), (t1::t2::t3::t4::t5::nil)) = Tupla5 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5)
  | listToTupla((x1::x2::x3::x4::x5::x6::nil), (t1::t2::t3::t4::t5::t6::nil)) = Tupla6 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6)
  | listToTupla((x1::x2::x3::x4::x5::x6::x7::nil), (t1::t2::t3::t4::t5::t6::t7::nil)) = Tupla7 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6, auxToType x7 t7)
  | listToTupla((x1::x2::x3::x4::x5::x6::x7::x8::nil), (t1::t2::t3::t4::t5::t6::t7::t8::nil)) = Tupla8 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6, auxToType x7 t7, auxToType x8 t8)
  | listToTupla((x1::x2::x3::x4::x5::x6::x7::x8::x9::nil), (t1::t2::t3::t4::t5::t6::t7::t8::t9::nil)) = Tupla9 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6, auxToType x7 t7, auxToType x8 t8, auxToType x9 t9)
  | listToTupla((x1::x2::x3::x4::x5::x6::x7::x8::x9::x10::nil), (t1::t2::t3::t4::t5::t6::t7::t8::t9::t10::nil)) = Tupla0 (auxToType x1 t1, auxToType x2 t2, auxToType x3 t3, auxToType x4 t4, auxToType x5 t5, auxToType x6 t6, auxToType x7 t7, auxToType x8 t8, auxToType x9 t9, auxToType x10 t10)
  | listToTupla((_),(_)) = raise SizeNotAllowed

fun getTupla(x, typeslist) = let val listValues = getCleanList x
                                in
                                listToTupla(listValues, typeslist)
                              end

fun getSample (x: string) = getListFrom x

(* fun isValueCorretType x "int" = Primitivo (Int_ (intFromString x))
  | isValueCorretType x "boolean"  = Primitivo(Boolean_ (boolFromString x))
  | isValueCorretType x "float" = Primitivo(Float_ (floatFromString x))
  | isValueCorretType x _ = Primitivo (String_ x) *)


(* Recebe um string que informa o path do arquivo
  e uma lista que contém os tipos para formar a Tupla. Ex.: ["string", "int"]

  Se o file tiver 2 linhas e 2 colunas:
  "teste", 1
  "teste2", 2

  essa função abri o arquivo e retorna uma lista de tuplas
  [(teste, 1), (teste2, 2)]
 *)
fun readlist (infile : string, typeslist: string list) = let
  val ins = TextIO.openIn infile
  fun loop ins =
   case TextIO.inputLine ins of
      SOME line => getTupla(line, typeslist) :: loop ins
    | NONE      => []
in
  loop ins before TextIO.closeIn ins
end

(* fun readFile filename =
    let val fd = TextIO.openIn filename
        val content = TextIO.inputAll fd handle e => (TextIO.closeIn fd; raise e)
        val _ = TextIO.closeIn fd
    in content end *)

(* Recebe uma string de uma path e o conteudo também como string*)
fun writeFile filename content =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end

(*Dado um tipo da tuple retorna o valor inicializado para aquele tipo,
  se é string retorna ""
  se é int retorna 0
  se é flaot retorna 0.0
  se é boll retorna false
*)
fun initTupleType "string" = Grammar.Primitivo(Grammar.String_ "")
  | initTupleType "int" = Grammar.Primitivo(Grammar.Int_ 0)
  | initTupleType "float" = Grammar.Primitivo(Grammar.Float_ 0.0)
  | initTupleType "bool" = Grammar.Primitivo(Grammar.Boolean_ false)
  | initTupleType _ = raise TypeError

(* Funções para inicializar uma tupla, passa os tipos ela retorna a tupla inicializada *)
fun tupleType(x1::x2::nil) = Tupla2 (initTupleType x1, initTupleType x2)
  | tupleType(x1::x2::x3::nil) = Tupla3 (initTupleType x1, initTupleType x2, initTupleType x3)
  | tupleType(x1::x2::x3::x4::nil) = Tupla4 (initTupleType x1, initTupleType x2, initTupleType x3, initTupleType x4)
  | tupleType(x1::x2::x3::x4::x5::nil) = Tupla5 (initTupleType x1, initTupleType x2, initTupleType x3, initTupleType x4, initTupleType x5)
  | tupleType(x1::x2::x3::x4::x5::x6::nil) = Tupla6 (initTupleType x1, initTupleType x2, initTupleType x3, initTupleType x4, initTupleType x5, initTupleType x6)
  | tupleType(x1::x2::x3::x4::x5::x6::x7::nil) = Tupla7 (initTupleType x1, initTupleType x2, initTupleType x3, initTupleType x4, initTupleType x5, initTupleType x6, initTupleType x7)
  | tupleType(x1::x2::x3::x4::x5::x6::x7::x8::nil) = Tupla8 (initTupleType x1, initTupleType x2, initTupleType x3, initTupleType x4, initTupleType x5, initTupleType x6, initTupleType x7, initTupleType x8)
  | tupleType(x1::x2::x3::x4::x5::x6::x7::x8::x9::nil) = Tupla9 (initTupleType x1, initTupleType x2, initTupleType x3, initTupleType x4, initTupleType x5, initTupleType x6, initTupleType x7, initTupleType x8, initTupleType x9)
  | tupleType(x1::x2::x3::x4::x5::x6::x7::x8::x9::x0::nil) = Tupla0 (initTupleType x1, initTupleType x2, initTupleType x3, initTupleType x4, initTupleType x5, initTupleType x6, initTupleType x7, initTupleType x8, initTupleType x9, initTupleType x0)
  | tupleType _ = raise SizeNotAllowed

fun extractColumn 1 (Tupla2(a,b)) = a
  | extractColumn 2 (Tupla2(a,b)) = b
  | extractColumn 1 (Tupla3(a,b,c)) = a
  | extractColumn 2 (Tupla3(a,b,c)) = b
  | extractColumn 3 (Tupla3(a,b,c)) = c
  | extractColumn 1 (Tupla4(a,b,c,d)) = a
  | extractColumn 2 (Tupla4(a,b,c,d)) = b
  | extractColumn 3 (Tupla4(a,b,c,d)) = c
  | extractColumn 4 (Tupla4(a,b,c,d)) = d
  | extractColumn 1 (Tupla5(a,b,c,d,e)) = a
  | extractColumn 2 (Tupla5(a,b,c,d,e)) = b
  | extractColumn 3 (Tupla5(a,b,c,d,e)) = c
  | extractColumn 4 (Tupla5(a,b,c,d,e)) = d
  | extractColumn 5 (Tupla5(a,b,c,d,e)) = e
  | extractColumn 1 (Tupla6(a,b,c,d,e,f)) = a
  | extractColumn 2 (Tupla6(a,b,c,d,e,f)) = b
  | extractColumn 3 (Tupla6(a,b,c,d,e,f)) = c
  | extractColumn 4 (Tupla6(a,b,c,d,e,f)) = d
  | extractColumn 5 (Tupla6(a,b,c,d,e,f)) = e
  | extractColumn 6 (Tupla6(a,b,c,d,e,f)) = f
  | extractColumn 1 (Tupla7(a,b,c,d,e,f,g)) = a
  | extractColumn 2 (Tupla7(a,b,c,d,e,f,g)) = b
  | extractColumn 3 (Tupla7(a,b,c,d,e,f,g)) = c
  | extractColumn 4 (Tupla7(a,b,c,d,e,f,g)) = d
  | extractColumn 5 (Tupla7(a,b,c,d,e,f,g)) = e
  | extractColumn 6 (Tupla7(a,b,c,d,e,f,g)) = f
  | extractColumn 7 (Tupla7(a,b,c,d,e,f,g)) = g
  | extractColumn 1 (Tupla8(a,b,c,d,e,f,g,h)) = a
  | extractColumn 2 (Tupla8(a,b,c,d,e,f,g,h)) = b
  | extractColumn 3 (Tupla8(a,b,c,d,e,f,g,h)) = c
  | extractColumn 4 (Tupla8(a,b,c,d,e,f,g,h)) = d
  | extractColumn 5 (Tupla8(a,b,c,d,e,f,g,h)) = e
  | extractColumn 6 (Tupla8(a,b,c,d,e,f,g,h)) = f
  | extractColumn 7 (Tupla8(a,b,c,d,e,f,g,h)) = g
  | extractColumn 8 (Tupla8(a,b,c,d,e,f,g,h)) = h
  | extractColumn 1 (Tupla9(a,b,c,d,e,f,g,h,i)) = a
  | extractColumn 2 (Tupla9(a,b,c,d,e,f,g,h,i)) = b
  | extractColumn 3 (Tupla9(a,b,c,d,e,f,g,h,i)) = c
  | extractColumn 4 (Tupla9(a,b,c,d,e,f,g,h,i)) = d
  | extractColumn 5 (Tupla9(a,b,c,d,e,f,g,h,i)) = e
  | extractColumn 6 (Tupla9(a,b,c,d,e,f,g,h,i)) = f
  | extractColumn 7 (Tupla9(a,b,c,d,e,f,g,h,i)) = g
  | extractColumn 8 (Tupla9(a,b,c,d,e,f,g,h,i)) = h
  | extractColumn 9 (Tupla9(a,b,c,d,e,f,g,h,i)) = i
  | extractColumn 1 (Tupla0(a,b,c,d,e,f,g,h,i,j)) = a
  | extractColumn 2 (Tupla0(a,b,c,d,e,f,g,h,i,j)) = b
  | extractColumn 3 (Tupla0(a,b,c,d,e,f,g,h,i,j)) = c
  | extractColumn 4 (Tupla0(a,b,c,d,e,f,g,h,i,j)) = d
  | extractColumn 5 (Tupla0(a,b,c,d,e,f,g,h,i,j)) = e
  | extractColumn 6 (Tupla0(a,b,c,d,e,f,g,h,i,j)) = f
  | extractColumn 7 (Tupla0(a,b,c,d,e,f,g,h,i,j)) = g
  | extractColumn 8 (Tupla0(a,b,c,d,e,f,g,h,i,j)) = h
  | extractColumn 9 (Tupla0(a,b,c,d,e,f,g,h,i,j)) = i
  | extractColumn 0 (Tupla0(a,b,c,d,e,f,g,h,i,j)) = j
  | extractColumn _ _ = raise TypeError


end
