structure Grammar =
struct

open Math;
exception VariableNotDeclared
exception SizeNotAllowed


datatype tipo_primitivo = Int_ of int
                        | String_ of string
                        | Float_ of real
                        | Boolean_ of bool

datatype tipo = Sample of (tipo list)
              | Primitivo of tipo_primitivo
              | Void

fun tokenize s = String.tokens (fn(c) => c = #",") (String.substring(s,1,(String.size s)-2))

fun toIntList nil = nil
   | toIntList is = List.map (fn(x) => Option.valOf (Int.fromString x)) is

fun toFloatList nil = nil
   | toFloatList is = List.map (fn(x) => Option.valOf (Real.fromString x)) is

fun toBoolList nil = nil
   | toBoolList is = List.map (fn(x) => Option.valOf (Bool.fromString x)) is


fun updateHt(ht: 'a AtomRedBlackMap.map,b,a: 'a): 'a AtomRedBlackMap.map =
    let
        val achou = AtomMap.find(ht,b)
    in
        case achou of
            NONE => raise VariableNotDeclared
            | SOME _ =>
                let
                    val (ht1,_) = AtomRedBlackMap.remove(ht, b)
                    val ht2 = AtomRedBlackMap.insert(ht1,b,a)
                in
                    ht2
                end
    end

fun show (Primitivo(Int_ i)) = Int.toString i
    | show (Primitivo(String_ s)) = s
    | show (Primitivo(Boolean_ b)) = Bool.toString b
    | show (Primitivo(Float_ b)) = Real.toString b
    | show (Sample nil) = "[]"
    | show (Sample (x::xs)) = (show x) ^ "," ^ String.concat(List.map show xs)
    | show _ = "null"


end
