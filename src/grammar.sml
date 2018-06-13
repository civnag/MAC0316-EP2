structure Grammar =
struct

open Math;
exception TypeMismatch
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

fun != (x: real, y:real):bool = Real.!=(x,y)

infix !=

fun oper("+", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Int_ (i+j))
   | oper("+", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Float_ (i+j))
   | oper("-", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Int_ (i-j))
   | oper("-", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Float_ (i-j))
   | oper("*", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Int_ (i*j))
   | oper("*", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Float_ (i*j))
   | oper("/", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Float_ (i/j))
   | oper(">=", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Boolean_ (i>=j))
   | oper("<=", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Boolean_ (i<=j))
   | oper(">", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Boolean_ (i>j))
   | oper("<", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Boolean_ (i<j))
   | oper("!=", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Boolean_ (i != j))
   | oper("==", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Boolean_ (not(i != j)))
   | oper(">=", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Boolean_ (i>=j))
   | oper("<=", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Boolean_ (i<=j))
   | oper(">", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Boolean_ (i>j))
   | oper("<", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Boolean_ (i<j))
   | oper("!=", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Boolean_ (not(i=j)))
   | oper("==", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Boolean_ (i=j))
   | oper("pow", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Float_ (Math.pow(i, j)))
   | oper("rt", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Float_ (Math.pow(i, 1.0/j)))
   | oper("neg",_,Primitivo(Int_ i)) = Primitivo(Int_ (0-i))
   | oper("neg",_,Primitivo(Float_ i)) = Primitivo(Float_ (0.0-i))
   | oper(_,_,_) = raise TypeMismatch

fun extractList (Sample x) = x
    | extractList _ = raise TypeMismatch

fun extractString (Primitivo(String_ x)) = x
    | extractString _ = raise TypeMismatch

fun extractBool (Primitivo(Boolean_ i)) = i
    | extractBool _ = raise TypeMismatch

fun extractInt (Primitivo(Int_ i)) = i
    | extractInt _ = raise TypeMismatch

fun extractFloat (Primitivo(Float_ i)) = i
    | extractFloat _ = raise TypeMismatch

fun typeof (Primitivo(Float_ _)) = "float"
    | typeof (Primitivo(Int_ _)) = "int"
    | typeof (Primitivo(Boolean_ _)) = "boolean"
    | typeof (Primitivo(String_ _)) = "string"
    | typeof (Sample nil) = "[]"
    | typeof (Sample (x::_)) = "sample of " ^ (typeof x)
    | typeof _ = "null"
end
