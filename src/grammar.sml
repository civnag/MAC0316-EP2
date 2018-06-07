structure Grammar =
struct 

exception TypeMismatch
exception VariableNotDeclared

datatype tipo_primitivo = Int_ of int 
                        | String_ of string 
                        | Float_ of real
                        | Boolean_ of bool

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

datatype tipo = Tupla2 of tipo * tipo
              | Tupla3 of tipo * tipo * tipo
              | Tupla4 of tipo * tipo * tipo * tipo
              | Tupla5 of tipo * tipo * tipo * tipo * tipo 
              | Tupla6 of tipo * tipo * tipo * tipo * tipo * tipo
              | Tupla7 of tipo * tipo * tipo * tipo * tipo * tipo * tipo
              | Tupla8 of tipo * tipo * tipo * tipo * tipo * tipo * tipo * tipo 
              | Tupla9 of tipo * tipo * tipo * tipo * tipo * tipo * tipo * tipo * tipo
              | Tupla0 of tipo * tipo * tipo * tipo * tipo * tipo * tipo * tipo * tipo * tipo
              | Sample of (tipo list)
              | Primitivo of tipo_primitivo;

fun show (Primitivo(Int_ i)) = Int.toString i
    | show (Primitivo(String_ s)) = s
    | show (Primitivo(Boolean_ b)) = Bool.toString b 
    | show (Primitivo(Float_ b)) = Real.toString b 
    | show (Tupla2 (a,b)) = "(" ^ show a ^ "," ^ show b ^ ")" 
    | show (Tupla3 (a,b,c)) = 
        "(" ^ show a ^ "," ^ show b ^ "," ^ show c ^ ")" 
    | show (Tupla4 (a,b,c,d)) = 
        "(" ^ show a ^ "," ^ show b ^ "," ^ show c ^ "," ^ show d ^ ")"
    | show (Tupla5 (a,b,c,d,e)) = 
        "(" ^ show a ^ "," ^ show b ^ "," ^ show c ^ "," ^ show d ^ "," ^ show e ^ ")" 
    | show (Tupla6 (a,b,c,d,e,f)) = 
        "(" ^ show a ^ "," ^ show b ^ "," ^ show c ^ "," ^ show d ^ "," ^ show e ^ "," ^ show f ^ ")"
    | show (Tupla7 (a,b,c,d,e,f,g)) = 
        "(" ^ show a ^ "," ^ show b ^ "," ^ show c ^ "," ^ show d ^ "," ^ show e ^ "," ^ show f ^ "," ^ show g ^ ")" 
    | show (Tupla8 (a,b,c,d,e,f,g,h)) = 
        "(" ^ show a ^ "," ^ show b ^ "," ^ show c ^ "," ^ show d ^ "," ^ show e ^ "," ^ show f ^ "," ^ show g ^ "," ^ show h ^")" 
    | show (Tupla9 (a,b,c,d,e,f,g,h,i)) =
        "(" ^ show a ^ "," ^ show b ^ "," ^ show c ^ "," ^ show d ^ "," ^ show e ^ "," ^ show f ^ "," ^ show g ^ "," ^ show h ^ "," ^ show i ^ ")" 
    | show (Tupla0 (a,b,c,d,e,f,g,h,i,j)) = 
        "(" ^ show a ^ "," ^ show b ^ "," ^ show c ^ "," ^ show d ^ "," ^ show e ^ "," ^ show f ^ "," ^ show g ^ "," ^ show h ^ "," ^ show i ^ "," ^ show j ^")" 
    | show (Sample nil) = "[]"
    | show (Sample (x::xs)) = (show x) ^ "," ^ String.concat(List.map show xs)

fun extractInt (Primitivo(Int_ i)) = i
    | extractInt _ = raise TypeMismatch

fun extractFloat (Primitivo(Float_ i)) = i
    | extractFloat _ = raise TypeMismatch

fun typeof (Primitivo(Float_ _)) = "float"
    | typeof (Primitivo(Int_ _)) = "int"
    | typeof (Primitivo(Boolean_ _)) = "bool"
    | typeof (Primitivo(String_ _)) = "string"
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
    | typeof (Sample nil) = "[]"
    | typeof (Sample (x::_)) = "[" ^ (typeof x) ^ "]"
end