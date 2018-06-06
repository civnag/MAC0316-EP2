structure Grammar =
struct 

exception TypeMismatch
exception VariableNotDeclared

datatype tipo_primitivo = Int_ of int 
                        | String_ of string 
                        | Float_ of real
                        | Boolean_ of bool

fun show (Int_ i) = Int.toString i
    | show (String_ s) = s
    | show (Boolean_ b) = Bool.toString b 
    | show (Float_ b) = Real.toString b 

fun extractInt (Int_ i) = i
    | extractInt _ = raise TypeMismatch

fun updateHt(a: 'a,b,ht: 'a AtomRedBlackMap.map): 'a AtomRedBlackMap.map = 
    let
        val achou = AtomMap.find(ht,b)
    in 
        case achou of
            NONE => raise VariableNotDeclared
            | SOME _ => 
                let 
                    val _ = AtomRedBlackMap.remove(ht, b) 
                    val _ = AtomRedBlackMap.insert(ht,b,a)
                in 
                    ht
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

type Var = string

datatype Exp = Const of tipo                                                         
             | Variable of Var
             | ToString of Exp
             | Print of Exp
             | Add of Exp * Exp                                                      
             | Sub of Exp * Exp                                                  
             | Mul of Exp * Exp                                                 
             | Div of Exp * Exp;  


infix :=

datatype Cmd = Seq of Cmd list | := of Var *  Exp | Action of Exp

type Memory = (Var, tipo) HashTable.hash_table;

type Decl = (Var * tipo) list;

type Program = string * Decl * Cmd;




end