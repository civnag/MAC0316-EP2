structure Grammar =
struct 

datatype tipo_primitivo = Int_ of int 
                        | String_ of string 
                        | Float_ of real
                        | Boolean_ of bool
                        | Void;

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