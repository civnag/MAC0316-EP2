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

fun showOpRel(NEQR) = "!="
  | showOpRel(EQR) = "="
  | showOpRel(GEQR) = ">="
  | showOpRel(LEQR) = "<="
  | showOpRel(GTR) = ">"
  | showOpRel(LTR) = "<"

fun eval(Const t,vars) = t 
  | eval(Var s,vars) = 
    let 
        val v = valOf(AtomMap.find(vars,Atom.atom s))
    in 
        v
    end
  | eval(Rel(oprel,e1,e2),vars) =
        let 
            val ee1 = eval(e1,vars)
            val ee2 = eval(e2,vars)
        in
            case (TypeChecker.typeof ee1,TypeChecker.typeof ee2) of
                  ("int","int") => TypeChecker.oper(showOpRel oprel,ee1,ee2)
                | ("float","float") => TypeChecker.oper(showOpRel oprel,ee1,ee2)
                | (_,_) => raise TypeChecker.TypeMismatch
        end

fun interpret((Print expr)::cs,vars,tps) = 
        let
            val evaluedExpr = eval(expr,vars)
        in
            print(Grammar.show evaluedExpr);
            interpret(cs,vars,tps)
        end
  | interpret(Assign(var,e)::cs,vars,tps) =
        let 
            val evaluedExpr = eval(e,vars)
            val varExists = AtomMap.inDomain(vars,Atom.atom var) 
            val t = valOf(AtomMap.find (tps,Atom.atom var))
            val sameType = (TypeChecker.isType evaluedExpr t)
        in 
            if (varExists andalso sameType) then
                interpret(cs,Grammar.updateHt(vars,Atom.atom var,evaluedExpr),tps)
            else 
                raise TypeChecker.TypeError
        end
  | interpret(If(e,c1,c2)::cs,vars,tps) = 
        let 
            val evaluedExpr = TypeChecker.extractBool(eval(e,vars))
        in 
            if evaluedExpr then 
                interpret(c1,vars,tps)
            else
                interpret(c2,vars,tps)
            ; interpret(cs,vars,tps)
        end
end