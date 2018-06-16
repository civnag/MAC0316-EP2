structure ParseTree = struct 

exception OperationNotSupported

open Grammar

datatype FuncOne = Mean | StdDev | Median | SumL | ProdL | ToString | ToInt | ToFloat
datatype BinOp = Add | Sub | Div | Mul | Not | And | Or | Pow | RT | Cov | Corr | Concat | LinReg
datatype OpRel = GTR | LTR | EQR | NEQR | GEQR | LEQR

datatype Expr = Const of tipo
              | FuncOne of FuncOne * Expr
              | FuncTwo of BinOp * Expr * Expr
              | Rel of OpRel * Expr * Expr
              | Var of string 

datatype Tree = Assign of string * Expr
              | Print of Expr 
              | If of Expr * (Tree list) * (Tree list) 
              | While of Expr * (Tree list) 
              | Null

type RoseTree = Tree list

fun getExprBoolTree("EEQ",e1,e2) = Rel(EQR,e1,e2)
    | getExprBoolTree("NEQ",e1,e2) = Rel(NEQR,e1,e2)
    | getExprBoolTree("LEQ",e1,e2) = Rel(LEQR,e1,e2)
    | getExprBoolTree("GEQ",e1,e2) = Rel(GEQR,e1,e2)
    | getExprBoolTree("LT",e1,e2) = Rel(LTR,e1,e2)
    | getExprBoolTree("GT",e1,e2) = Rel(GTR,e1,e2)
    | getExprBoolTree(_,_,_) = raise OperationNotSupported

fun insere(hm,n,"int") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Int_ 0))
    | insere(hm,n,"string") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.String_ ""))
    | insere(hm,n,"float") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Float_ 0.0))
    | insere(hm,n,"boolean") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Boolean_ false))
    | insere(hm,n,_) = AtomMap.insert(hm, n, Grammar.Void)
end