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

datatype tipo_tupla = tipo_tupla2 of tipo_primitivo * tipo_primitivo
                    | tipo_tupla3 of tipo_primitivo * tipo_primitivo * tipo_primitivo
                    | tipo_tupla4 of tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo
                    | tipo_tupla5 of tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo
                    | tipo_tupla6 of tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo
                    | tipo_tupla7 of tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo
                    | tipo_tupla8 of tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo
                    | tipo_tupla9 of tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo
                    | tipo_tupla0 of tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo * tipo_primitivo;

datatype prim_or_tupla = TP of tipo_primitivo 
                       | TT of tipo_tupla;

datatype sample_of = sample_of of prim_or_tupla list;

datatype tipo = Tupla of tipo_tupla 
              | Sample of sample_of
              | Primitivo of tipo_primitivo;

fun show (Primitivo(Int_ i)) = Int.toString i
    | show (Primitivo(String_ s)) = s
    | show (Primitivo(Boolean_ b)) = Bool.toString b 
    | show (Primitivo(Float_ b)) = Real.toString b 
    | show (Sample(sample_of ((TP x)::nil))) = show (Primitivo x) 
    | show (Sample(sample_of ((TP x)::xs))) = show (Primitivo x) ^ "," ^ show (Sample (sample_of xs))
    | show _ = ""

fun extractInt (Primitivo(Int_ i)) = i
    | extractInt _ = raise TypeMismatch

fun extractFloat (Primitivo(Float_ i)) = i
    | extractFloat _ = raise TypeMismatch

fun typeof (Primitivo(Float_ _)) = "float"
    | typeof (Primitivo(Int_ _)) = "int"
    | typeof (Primitivo(Boolean_ _)) = "bool"
    | typeof (Primitivo(String_ _)) = "string"
    | typeof _ = ""

end