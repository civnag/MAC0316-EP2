
structure ParseTree = struct

exception OperationNotSupported

open Grammar
open Helper

datatype UnOp = Mean | StdDev | Median | SumL | ProdL | ToString | ToInt | ToFloat | Variance
datatype BinOp = Add | Sub | Div | Mul | And | Or | Pow | RT | Cov | Corr | Concat | LinReg | GetFloat | GetInt | GetString
datatype OpRel = GTR | LTR | EQR | NEQR | GEQR | LEQR

datatype Expr = Const of tipo
              | FuncOne of UnOp * Expr
              | FuncTwo of BinOp * Expr * Expr
              | Rel of OpRel * Expr * Expr
              | Var of string


datatype Tree = Assign of string * Expr
              | Print of Expr
              | If of Expr * (Tree list) * (Tree list)
              | While of Expr * (Tree list)
              | Case1 of Expr * Expr * 
                        (Tree list) 
              | Case2 of Expr * Expr * Expr * 
                        (Tree list) * (Tree list)
              | Case3 of Expr * Expr * Expr * Expr *
                        (Tree list) * (Tree list) * (Tree list)
              | Case4 of Expr * Expr * Expr * Expr * Expr *
                        (Tree list) * (Tree list) * (Tree list) * (Tree list)
              | Case5 of Expr * Expr * Expr * Expr * Expr * Expr * 
                        (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list)
              | Case6 of Expr * Expr * Expr * Expr * Expr * Expr * Expr * 
                        (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list)
              | Case7 of Expr * Expr * Expr * Expr * Expr * Expr * Expr * Expr * 
                        (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list)
              | Case8 of Expr * Expr * Expr * Expr * Expr * Expr * Expr * Expr * Expr * 
                        (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list)
              | Case9 of Expr * Expr * Expr * Expr * Expr * Expr * Expr * Expr * Expr * Expr * 
                        (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list)
              | Case10 of Expr * Expr * Expr * Expr * Expr * Expr * Expr * Expr * Expr * Expr * Expr * 
                        (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list) * (Tree list)
              | Null

type RoseTree = Tree list

fun getBinaryFun("+", e1, e2) = FuncTwo(Add, e1, e2)
  | getBinaryFun("-", e1, e2) = FuncTwo(Sub, e1, e2)
  | getBinaryFun("/", e1, e2) = FuncTwo(Div, e1, e2)
  | getBinaryFun("*", e1, e2) = FuncTwo(Mul, e1, e2)
  | getBinaryFun("&&", e1, e2) = FuncTwo(And, e1, e2)
  | getBinaryFun("||", e1, e2) = FuncTwo(Or, e1, e2)
  | getBinaryFun("pow", e1, e2) = FuncTwo(Pow, e1, e2)
  | getBinaryFun("rt", e1, e2) = FuncTwo(RT, e1, e2)
  | getBinaryFun("covariance", e1, e2) = FuncTwo(Cov, e1, e2)
  | getBinaryFun("correlation", e1, e2) = FuncTwo(Corr, e1, e2)
  | getBinaryFun("++", e1, e2) = FuncTwo(Concat, e1, e2)
  | getBinaryFun("linearRegression", e1, e2) = FuncTwo(LinReg, e1, e2)
  | getBinaryFun("getFloat", e1, e2) = FuncTwo(GetFloat, e1, e2)
  | getBinaryFun("getString", e1, e2) = FuncTwo(GetString, e1, e2)
  | getBinaryFun("getInt", e1, e2) = FuncTwo(GetInt, e1, e2)
  | getBinaryFun(_,_,_) = raise OperationNotSupported
  handle e => (print "Exception: ";exnName e; e)

fun floatListToSampleExpr(fl) = Const(Sample (List.map (fn(x) => Primitivo(Float_ x)) fl))

fun intListToSampleExpr(il) = Const(Sample (List.map (fn(x) => Primitivo(Int_ x)) il))

fun stringListToSampleExpr(il) = Const(Sample (List.map (fn(x) => Primitivo(String_ x)) il))

fun boolListToSampleExpr(il) = Const(Sample (List.map (fn(x) => Primitivo(Boolean_ x)) il))

fun showBinOp(Add) = "+"
  | showBinOp(Sub) = "-"
  | showBinOp(Div) = "/"
  | showBinOp(Mul) = "*"
  | showBinOp(And) = "&&"
  | showBinOp(Or) = "||"
  | showBinOp(Pow) = "pow"
  | showBinOp(RT) = "rt"
  | showBinOp(Cov) = "covariance"
  | showBinOp(Corr) = "correlation"
  | showBinOp(Concat) = "++"
  | showBinOp(LinReg) = "linearRegression"
  | showBinOp(GetFloat) = "getFloat"
  | showBinOp(GetInt) = "getInt"
  | showBinOp(GetString) = "getString"


fun getFunctionOne("mean", e1) = FuncOne(Mean,e1)
  | getFunctionOne("stdDeviation", e1) = FuncOne(StdDev,e1)
  | getFunctionOne("variance", e1) = FuncOne(Variance,e1)
  | getFunctionOne("median", e1) = FuncOne(Median,e1)
  | getFunctionOne("sum", e1) = FuncOne(SumL,e1)
  | getFunctionOne("prod", e1) = FuncOne(ProdL,e1)
  | getFunctionOne("toString", e1) = FuncOne(ToString,e1)
  | getFunctionOne("toInt", e1) = FuncOne(ToInt,e1)
  | getFunctionOne("toFloat", e1) = FuncOne(ToFloat,e1)
  | getFunctionOne(_, _) = raise OperationNotSupported
  handle e => (print "Exception: ";exnName e; e)

fun getExprBoolTree("==",e1,e2) = Rel(EQR,e1,e2)
    | getExprBoolTree("!=",e1,e2) = Rel(NEQR,e1,e2)
    | getExprBoolTree("<=",e1,e2) = Rel(LEQR,e1,e2)
    | getExprBoolTree(">=",e1,e2) = Rel(GEQR,e1,e2)
    | getExprBoolTree("<",e1,e2) = Rel(LTR,e1,e2)
    | getExprBoolTree(">",e1,e2) = Rel(GTR,e1,e2)
    | getExprBoolTree(_,_,_) = raise OperationNotSupported

fun insere(hm,n,"int") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Int_ 0))
    | insere(hm,n,"string") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.String_ ""))
    | insere(hm,n,"float") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Float_ 0.0))
    | insere(hm,n,"boolean") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Boolean_ false))
    | insere(hm,n,_) = AtomMap.insert(hm, n, Grammar.Void)

fun showOpRel(NEQR) = "!="
  | showOpRel(EQR) = "=="
  | showOpRel(GEQR) = ">="
  | showOpRel(LEQR) = "<="
  | showOpRel(GTR) = ">"
  | showOpRel(LTR) = "<"

fun showFunctionOne(Mean) = "mean"
  | showFunctionOne(StdDev) = "stdDeviation"
  | showFunctionOne(Median) = "median"
  | showFunctionOne(SumL) = "sum"
  | showFunctionOne(ProdL) = "prod"
  | showFunctionOne(ToInt) = "toInt"
  | showFunctionOne(ToString) = "toString"
  | showFunctionOne(ToFloat) = "toFloat"
  | showFunctionOne(Variance) = "variance"

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
  | eval(FuncOne(func, e1), vars) =
        let
            val ee1 = eval(e1, vars)
            (* val _ = print("Type 1: " ^ (TypeChecker.typeof ee1) ^ " fun " ^ showFunctionOne func ^ "\n") *)
        in
            case (TypeChecker.typeof ee1) of
                ("sample of float") => TypeChecker.functionOne("sample of float", showFunctionOne func, ee1)
              | ("sample of int") => TypeChecker.functionOne("sample of int", showFunctionOne func, ee1)
              | ("float") => TypeChecker.functionOne("", showFunctionOne func, ee1)
              | ("int") => TypeChecker.functionOne("", showFunctionOne func, ee1)
              | ("string") => TypeChecker.functionOne("", showFunctionOne func, ee1)
              | ("boolean") => TypeChecker.functionOne("", showFunctionOne func, ee1)
              | (_) => TypeChecker.functionOne("", showFunctionOne func, ee1)
        end
  | eval(FuncTwo(binop, e1, e2), vars) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
            (* val _ = print("Type 2: " ^ (TypeChecker.typeof ee1)^ " " ^ (TypeChecker.typeof ee2) ^ " " ^ (showBinOp binop) ^"\n") *)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2) of
                ("sample of float", "sample of float") => TypeChecker.statistics("sample of float", showBinOp binop, ee1, ee2)
              | ("sample of int", "sample of int") => TypeChecker.statistics("sample of int", showBinOp binop, ee1, ee2)
              | ("sample of float", "int") => TypeChecker.functionTwo(showBinOp binop,ee1,ee2)
              | ("sample of int", "int") => TypeChecker.functionTwo(showBinOp binop,ee1,ee2)
              | ("string","sample of string") => TypeChecker.functionTwo(showBinOp binop,ee1,ee2)
              | ("sample of string","int") => TypeChecker.functionTwo(showBinOp binop,ee1,ee2)
              | ("float", "float") => TypeChecker.oper(showBinOp binop, ee1, ee2)
              | ("int", "int") => TypeChecker.oper(showBinOp binop, ee1, ee2)
              | ("string", "string") => TypeChecker.oper(showBinOp binop, ee1, ee2)
              | ("boolean", "boolean") => TypeChecker.oper(showBinOp binop, ee1, ee2)
              | (_, _) => raise TypeChecker.TypeMismatch
        end


fun interpret((Print expr),vars,tps) =
        let
            val evaluedExpr = eval(expr,vars)
        in
            print(Grammar.show evaluedExpr ^ "\n");
            vars
        end
  | interpret(Assign(var,e),vars,tps) =
        let
            val evaluedExpr = eval(e,vars)
            val varExists = AtomMap.inDomain(vars,Atom.atom var)
            val t = valOf(AtomMap.find (tps,Atom.atom var))
            val sameType = (TypeChecker.isType evaluedExpr t)
        in
            if (varExists andalso sameType) then
                Grammar.updateHt(vars,Atom.atom var,evaluedExpr)
            else
                raise TypeChecker.TypeError
        end
  | interpret(If(e,c1,c2),vars,tps) =
        let
            val evaluedExpr = TypeChecker.extractBool(eval(e,vars))
        in
            if evaluedExpr then
                programa(c1, vars, tps)
            else
                programa(c2, vars, tps);
            vars
        end
  | interpret(While(e,c1),vars,tps) =
        let
            val evaluedExpr = TypeChecker.extractBool(eval(e,vars))
        in
            if evaluedExpr then
              let val newVars = programa(c1, vars, tps) in interpret(While(e,c1), newVars, tps) end
            else
              vars
        end
  | interpret(Case1(e1, e2, c), vars, tps) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2) of
                ("boolean", "boolean") => 
                    if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee2) then 
                        programa(c, vars, tps)
                    else 
                        vars
              | ("int", "int") =>
                    if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee2) then 
                        programa(c, vars, tps)
                    else 
                        vars
              | ("float", "float") =>
                    if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee2)) then 
                        programa(c, vars, tps)
                    else 
                        vars
        end 
  | interpret(Case2(e1, e2, e3, c1, c2), vars, tps) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
            val ee3 = eval(e3, vars)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2, TypeChecker.typeof ee3) of
                ("boolean", "boolean", "boolean") => 
                    if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee3) then 
                        programa(c2, vars, tps)
                    else 
                        vars
              | ("int", "int", "int") =>
                    if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee3) then 
                        programa(c2, vars, tps)
                    else 
                        vars
              | ("float", "float", "float") =>
                    if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee2)) then 
                        programa(c1, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee3)) then 
                        programa(c2, vars, tps)
                    else 
                        vars
        end
  | interpret(Case3(e1, e2, e3, e4, c1, c2, c3), vars, tps) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
            val ee3 = eval(e3, vars)
            val ee4 = eval(e4, vars)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2, TypeChecker.typeof ee3) of
                ("boolean", "boolean", "boolean") => 
                    if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee4) then 
                        programa(c3, vars, tps)
                    else 
                        vars
              | ("int", "int", "int") =>
                    if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee4) then 
                        programa(c3, vars, tps)
                    else 
                        vars
              | ("float", "float", "float") =>
                    if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee2)) then 
                        programa(c1, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee3)) then 
                        programa(c2, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee4)) then 
                        programa(c3, vars, tps)    
                    else 
                        vars
        end
  | interpret(Case4(e1, e2, e3, e4, e5, c1, c2, c3, c4), vars, tps) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
            val ee3 = eval(e3, vars)
            val ee4 = eval(e4, vars)
            val ee5 = eval(e5, vars)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2, TypeChecker.typeof ee3) of
                ("boolean", "boolean", "boolean") => 
                    if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee5) then 
                        programa(c4, vars, tps)
                    else 
                        vars
              | ("int", "int", "int") =>
                    if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee5) then 
                        programa(c4, vars, tps)
                    else 
                        vars
              | ("float", "float", "float") =>
                    if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee2)) then 
                        programa(c1, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee3)) then 
                        programa(c2, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee4)) then 
                        programa(c3, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee5)) then 
                        programa(c4, vars, tps)  
                    else 
                        vars
        end
  | interpret(Case5(e1, e2, e3, e4, e5, e6, c1, c2, c3, c4, c5), vars, tps) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
            val ee3 = eval(e3, vars)
            val ee4 = eval(e4, vars)
            val ee5 = eval(e5, vars)
            val ee6 = eval(e6, vars)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2, TypeChecker.typeof ee3) of
                ("boolean", "boolean", "boolean") => 
                    if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee5) then 
                        programa(c4, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee6) then 
                        programa(c5, vars, tps)
                    else 
                        vars
              | ("int", "int", "int") =>
                    if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee5) then 
                        programa(c4, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee6) then 
                        programa(c5, vars, tps)
                    else 
                        vars
              | ("float", "float", "float") =>
                    if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee2)) then 
                        programa(c1, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee3)) then 
                        programa(c2, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee4)) then 
                        programa(c3, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee5)) then 
                        programa(c4, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee6)) then 
                        programa(c5, vars, tps)  
                    else 
                        vars
        end
  | interpret(Case6(e1, e2, e3, e4, e5, e6, e7, c1, c2, c3, c4, c5, c6), vars, tps) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
            val ee3 = eval(e3, vars)
            val ee4 = eval(e4, vars)
            val ee5 = eval(e5, vars)
            val ee6 = eval(e6, vars)
            val ee7 = eval(e7, vars)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2, TypeChecker.typeof ee3) of
                ("boolean", "boolean", "boolean") => 
                    if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee5) then 
                        programa(c4, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee6) then 
                        programa(c5, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee7) then 
                        programa(c6, vars, tps)
                    else 
                        vars
              | ("int", "int", "int") =>
                    if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee5) then 
                        programa(c4, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee6) then 
                        programa(c5, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee7) then 
                        programa(c6, vars, tps)
                    else 
                        vars
              | ("float", "float", "float") =>
                    if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee2)) then 
                        programa(c1, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee3)) then 
                        programa(c2, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee4)) then 
                        programa(c3, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee5)) then 
                        programa(c4, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee6)) then 
                        programa(c5, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee7)) then 
                        programa(c6, vars, tps) 
                    else 
                        vars
        end
  | interpret(Case7(e1, e2, e3, e4, e5, e6, e7, e8, c1, c2, c3, c4, c5, c6, c7), vars, tps) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
            val ee3 = eval(e3, vars)
            val ee4 = eval(e4, vars)
            val ee5 = eval(e5, vars)
            val ee6 = eval(e6, vars)
            val ee7 = eval(e7, vars)
            val ee8 = eval(e8, vars)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2, TypeChecker.typeof ee3) of
                ("boolean", "boolean", "boolean") => 
                    if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee5) then 
                        programa(c4, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee6) then 
                        programa(c5, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee7) then 
                        programa(c6, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee8) then 
                        programa(c7, vars, tps)
                    else 
                        vars
              | ("int", "int", "int") =>
                    if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee5) then 
                        programa(c4, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee6) then 
                        programa(c5, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee7) then 
                        programa(c6, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee8) then 
                        programa(c7, vars, tps)
                    else 
                        vars
              | ("float", "float", "float") =>
                    if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee2)) then 
                        programa(c1, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee3)) then 
                        programa(c2, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee4)) then 
                        programa(c3, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee5)) then 
                        programa(c4, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee6)) then 
                        programa(c5, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee7)) then 
                        programa(c6, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee8)) then 
                        programa(c7, vars, tps)   
                    else 
                        vars
        end
  | interpret(Case8(e1, e2, e3, e4, e5, e6, e7, e8, e9, c1, c2, c3, c4, c5, c6, c7, c8), vars, tps) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
            val ee3 = eval(e3, vars)
            val ee4 = eval(e4, vars)
            val ee5 = eval(e5, vars)
            val ee6 = eval(e6, vars)
            val ee7 = eval(e7, vars)
            val ee8 = eval(e8, vars)
            val ee9 = eval(e9, vars)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2, TypeChecker.typeof ee3) of
                ("boolean", "boolean", "boolean") => 
                    if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee5) then 
                        programa(c4, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee6) then 
                        programa(c5, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee7) then 
                        programa(c6, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee8) then 
                        programa(c7, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee9) then 
                        programa(c8, vars, tps)
                    else 
                        vars
              | ("int", "int", "int") =>
                    if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee5) then 
                        programa(c4, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee6) then 
                        programa(c5, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee7) then 
                        programa(c6, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee8) then 
                        programa(c7, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee9) then 
                        programa(c8, vars, tps)
                    else 
                        vars
              | ("float", "float", "float") =>
                    if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee2)) then 
                        programa(c1, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee3)) then 
                        programa(c2, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee4)) then 
                        programa(c3, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee5)) then 
                        programa(c4, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee6)) then 
                        programa(c5, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee7)) then 
                        programa(c6, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee8)) then 
                        programa(c7, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee9)) then 
                        programa(c8, vars, tps) 
                    else 
                        vars
        end
  | interpret(Case9(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, c1, c2, c3, c4, c5, c6, c7, c8, c9), vars, tps) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
            val ee3 = eval(e3, vars)
            val ee4 = eval(e4, vars)
            val ee5 = eval(e5, vars)
            val ee6 = eval(e6, vars)
            val ee7 = eval(e7, vars)
            val ee8 = eval(e8, vars)
            val ee9 = eval(e9, vars)
            val ee10 = eval(e10, vars)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2, TypeChecker.typeof ee3) of
                ("boolean", "boolean", "boolean") => 
                    if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee5) then 
                        programa(c4, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee6) then 
                        programa(c5, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee7) then 
                        programa(c6, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee8) then 
                        programa(c7, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee9) then 
                        programa(c8, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee10) then 
                        programa(c9, vars, tps)
                    else 
                        vars
              | ("int", "int", "int") =>
                    if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee5) then 
                        programa(c4, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee6) then 
                        programa(c5, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee7) then 
                        programa(c6, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee8) then 
                        programa(c7, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee9) then 
                        programa(c8, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee10) then 
                        programa(c9, vars, tps)
                    else 
                        vars
              | ("float", "float", "float") =>
                    if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee2)) then 
                        programa(c1, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee3)) then 
                        programa(c2, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee4)) then 
                        programa(c3, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee5)) then 
                        programa(c4, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee6)) then 
                        programa(c5, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee7)) then 
                        programa(c6, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee8)) then 
                        programa(c7, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee9)) then 
                        programa(c8, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee10)) then 
                        programa(c9, vars, tps)    
                    else 
                        vars
        end
  | interpret(Case10(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10), vars, tps) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
            val ee3 = eval(e3, vars)
            val ee4 = eval(e4, vars)
            val ee5 = eval(e5, vars)
            val ee6 = eval(e6, vars)
            val ee7 = eval(e7, vars)
            val ee8 = eval(e8, vars)
            val ee9 = eval(e9, vars)
            val ee10 = eval(e10, vars)
            val ee11 = eval(e11, vars)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2, TypeChecker.typeof ee3) of
                ("boolean", "boolean", "boolean") => 
                    if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee5) then 
                        programa(c4, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee6) then 
                        programa(c5, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee7) then 
                        programa(c6, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee8) then 
                        programa(c7, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee9) then 
                        programa(c8, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee10) then 
                        programa(c9, vars, tps)
                    else if TypeChecker.extractBool(ee1) = TypeChecker.extractBool(ee11) then 
                        programa(c10, vars, tps)
                    else 
                        vars
              | ("int", "int", "int") =>
                    if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee2) then 
                        programa(c1, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee3) then 
                        programa(c2, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee4) then 
                        programa(c3, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee5) then 
                        programa(c4, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee6) then 
                        programa(c5, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee7) then 
                        programa(c6, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee8) then 
                        programa(c7, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee9) then 
                        programa(c8, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee10) then 
                        programa(c9, vars, tps)
                    else if TypeChecker.extractInt(ee1) = TypeChecker.extractInt(ee11) then 
                        programa(c10, vars, tps)
                    else 
                        vars
              | ("float", "float", "float") =>
                    if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee2)) then 
                        programa(c1, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee3)) then 
                        programa(c2, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee4)) then 
                        programa(c3, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee5)) then 
                        programa(c4, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee6)) then 
                        programa(c5, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee7)) then 
                        programa(c6, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee8)) then 
                        programa(c7, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee9)) then 
                        programa(c8, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee10)) then 
                        programa(c9, vars, tps)
                    else if Real.==(TypeChecker.extractFloat(ee1), TypeChecker.extractFloat(ee11)) then 
                        programa(c10, vars, tps)    
                    else 
                        vars
        end
  | interpret(cs,vars,tps) = vars
  (* handle e => (print ("Exception: " ^ exnName e); ()) *)

and programa(x::ls, vars, tps) = let val varsNew = interpret(x, vars, tps) in programa(ls, varsNew, tps) end
  | programa([], vars, tps) = vars


end
