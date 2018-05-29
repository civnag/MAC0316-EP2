datatype tipo_primitivo = Int_ of int 
                        | String_ of string 
                        | Float_ of real
                        | Boolean_ of bool;

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

type Var = tipo * string;  

datatype Exp = Const of tipo                                                         
             | Variable of Var                                                   
             | Add of Exp * Exp                                                      
             | Sub of Exp * Exp                                                  
             | Mul of Exp * Exp                                                 
             | Div of Exp * Exp;  

infix 1 :=

datatype Cmd = Title of string | Seq of Cmd list | := of Var * Exp;  


structure Aux =
struct 

fun algo(x) = x+1;


end