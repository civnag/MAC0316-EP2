structure TypeChecker =
struct

open Statistics
open Grammar

exception TypeMismatch
exception FunctionOneNotImplemented
exception FunctionTwoNotImplemented
exception StatisticsNotImplemented
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
    | typeof (Tupla2 (a,b)) = "(" ^ typeof a ^ "," ^ typeof b ^ ")"
    | typeof (Tupla3 (a,b,c)) = "(" ^ typeof a ^ "," ^ typeof b ^ "," ^ typeof c ^ ")"
    | typeof (Tupla4 (a,b,c,d)) = 
        "(" ^ typeof a ^ "," ^ typeof b ^ "," ^ typeof c ^ "," ^ typeof d ^ ")"
    | typeof (Tupla5 (a,b,c,d,e)) = 
        "(" ^ typeof a ^ "," ^ typeof b ^ "," ^ typeof c ^ "," ^ typeof d ^ "," ^ typeof e ^")"
    | typeof (Tupla6 (a,b,c,d,e,f)) = 
        "(" ^ typeof a ^ "," ^ typeof b ^ "," 
            ^ typeof c ^ "," ^ typeof d ^ "," ^ typeof e ^"," ^ typeof f ^")"
    | typeof (Tupla7 (a,b,c,d,e,f,g)) = 
        "(" ^ typeof a ^ "," ^ typeof b ^ "," 
            ^ typeof c ^ "," ^ typeof d ^ "," ^ typeof e ^"," ^ typeof f ^"," ^ typeof g ^")"
    | typeof (Tupla8 (a,b,c,d,e,f,g,h)) = 
        "(" ^ typeof a ^ "," ^ typeof b ^ "," ^ typeof c ^ "," ^ typeof d ^ "," ^ typeof e ^"," 
            ^ typeof f ^"," ^ typeof g ^"," ^ typeof h ^")"
    | typeof (Tupla9 (a,b,c,d,e,f,g,h,i)) = 
        "(" ^ typeof a ^ "," ^ typeof b ^ "," ^ typeof c 
            ^ "," ^ typeof d ^ "," ^ typeof e ^","
            ^ typeof f ^"," ^ typeof g ^"," 
            ^ typeof h ^"," ^ typeof i ^")"
    | typeof (Tupla0 (a,b,c,d,e,f,g,h,i,j)) = 
        "(" ^ typeof a ^ "," ^ typeof b ^ "," ^ typeof c ^ "," ^ typeof d ^ "," ^ typeof e ^ "," 
            ^ typeof f ^ "," ^ typeof g ^"," ^ typeof h ^"," 
            ^ typeof i ^"," ^ typeof j ^")"
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
   | oper("++",Primitivo(String_ l),Primitivo(String_ m)) = Primitivo(String_ ("\""^ String.implode((List.filter (fn(x) => not(x = #"\"")) (String.explode(l ^ m)))) ^ "\""))
   | oper(_,_,_) = raise FunctionTwoNotImplemented

fun exprTypes e1 e2 = (typeof e1) = (typeof e2)
fun isType e1 t = (typeof e1) = t

fun statistics("correlation", Sample(x), Sample(y)) = Primitivo(Float_ (Statistics.correlation((List.map extractFloat x), (List.map extractFloat y))))
  | statistics("covariance", Sample(x), Sample(y)) = Primitivo(Float_ (Statistics.covariance((List.map extractFloat x), (List.map extractFloat y))))
  | statistics("linearRegression", Sample(x), Sample(y)) = Primitivo(String_ (Statistics.linearRegression((List.map extractFloat x), (List.map extractFloat y))))
  | statistics(_, _, _) = raise StatisticsNotImplemented
  handle e => (print ("Exception: " ^ exnName e); e)


fun functionTwo("getFloat",Sample ls,Primitivo(Int_ i)) = List.nth(ls,i)
  | functionTwo("getInt",Sample ls,Primitivo(Int_ i)) = List.nth(ls,i)
  | functionTwo("getString",Sample ls,Primitivo(Int_ i)) = List.nth(ls,i)
  | functionTwo(_,_,_) = raise FunctionTwoNotImplemented

fun functionThree("substring",Primitivo(String_ ls),Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo(String_(String.substring(ls,i,j)))
  | functionThree(_,_,_,_) = raise FunctionTwoNotImplemented

fun functionOne("mean", Sample(x)) = Primitivo(Float_ (Statistics.standardDeviation (List.map extractFloat x)))
  | functionOne("stdDeviation", Sample(x)) = Primitivo(Float_ (Statistics.standardDeviation (List.map extractFloat x)))
  | functionOne("variance", Sample(x)) = Primitivo(Float_ (Statistics.variance (List.map extractFloat x)))
  | functionOne("median", Sample(x)) = Primitivo(Float_ (Statistics.median (List.map extractFloat x)))
  | functionOne("toString", x ) = Primitivo (String_ (Grammar.show x))
  | functionOne("toFloat", Primitivo(Int_ value)) = Primitivo(Float_ (Real.fromInt value))
  | functionOne("toInt", Primitivo(Float_ value)) = Primitivo(Int_ (Real.trunc value))
  | functionOne("sum", Sample(x)) = raise FunctionOneNotImplemented
  | functionOne("prod", Sample(x)) = raise FunctionOneNotImplemented
  | functionOne(_, _) = raise TypeMismatch

end
