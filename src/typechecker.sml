structure TypeChecker =
struct 

open Grammar

exception TypeMismatch
exception TypeError

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

fun exprTypes e1 e2 = (typeof e1) = (typeof e2)
fun isType e1 t = (typeof e1) = t

end