structure ParseTree = struct 

open Grammar

datatype FuncOne = Mean | StdDev | Median | SumL | ProdL | ToString | ToInt | ToFloat
datatype BinOp = Add | Sub | Div | Mul | Not | And | Or | Pow | Cov | Corr | Concat | LinReg
datatype OpRel = GTR | LTR | EQR | NEQR | GEQR | LEQR

datatype Expr = Const of tipo
              | FuncOne of FuncOne * Expr
              | FuncTwo of BinOp * Expr * Expr
              | Rel of OpRel * Expr * Expr
              | Var of string 

datatype Tree = Assign of string * Expr
              | Print of Expr 
              | If of bool * (Tree list) * (Tree list) 
              | While of bool * (Tree list) 
              | Null

type RoseTree = Tree list

fun insere(hm,n,"int") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Int_ 0))
    | insere(hm,n,"string") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.String_ ""))
    | insere(hm,n,"float") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Float_ 0.0))
    | insere(hm,n,"boolean") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Boolean_ false))
    | insere(hm,n,_) = AtomMap.insert(hm, n, Grammar.Void)
end