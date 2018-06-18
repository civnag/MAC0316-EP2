structure 
DarwinTokens = struct

    datatype token = EOF
      | CONCAT
      | KW_TOINT
      | KW_TOFLOAT
      | KW_GETI
      | VOID
      | KW_LINREG
      | KW_SUBS
      | KW_COV
      | KW_GETF
      | KW_POW
      | KW_RT
      | KW_VAR
      | KW_STDEV
      | KW_MEDIAN
      | KW_CORR
      | KW_MEAN
      | KW_TOSTRING
      | KW_END
      | KW_DO
      | KW_WHILE
      | KW_ELSE
      | KW_THEN
      | KW_IF
      | KW_GETS
      | EMPTY
      | KW_PROD
      | KW_SUM
      | KW_terminate
      | KW_endvars
      | KW_Print
      | STR of string
      | KW_commands
      | DOTDOT
      | TIPO of string
      | SEMI
      | KW_variables
      | NEQ
      | GEQ
      | LEQ
      | LT
      | GT
      | SPACE
      | NOT
      | OR
      | AND
      | BOOL of bool
      | RP
      | LP
      | SBOOL of (Bool.bool list)
      | COMMA
      | MINUS
      | DIV
      | TIMES
      | SFLOAT of (Real.real list)
      | EEQ
      | DOT
      | PLUS
      | EQ
      | SINT of (Int.int list)
      | REAL of Real.real
      | NUM of Int.int
      | ID of string
      | SSTRING of (string list)
      | KW_title
      | KW_in
      | KW_let

    val allToks = [EOF, CONCAT, KW_TOINT, KW_TOFLOAT, KW_GETI, VOID, KW_LINREG, KW_SUBS, KW_COV, KW_GETF, KW_POW, KW_RT, KW_VAR, KW_STDEV, KW_MEDIAN, KW_CORR, KW_MEAN, KW_TOSTRING, KW_END, KW_DO, KW_WHILE, KW_ELSE, KW_THEN, KW_IF, KW_GETS, EMPTY, KW_PROD, KW_SUM, KW_terminate, KW_endvars, KW_Print, KW_commands, DOTDOT, SEMI, KW_variables, NEQ, GEQ, LEQ, LT, GT, SPACE, NOT, OR, AND, RP, LP, COMMA, MINUS, DIV, TIMES, EEQ, DOT, PLUS, EQ, KW_title, KW_in, KW_let]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (CONCAT) => "++"
  | (KW_TOINT) => "toInt"
  | (KW_TOFLOAT) => "toFloat"
  | (KW_GETI) => "getInt"
  | (VOID) => "void"
  | (KW_LINREG) => "linearRegression"
  | (KW_SUBS) => "subSample"
  | (KW_COV) => "covariance"
  | (KW_GETF) => "getFloat"
  | (KW_POW) => "pow"
  | (KW_RT) => "rt"
  | (KW_VAR) => "variance"
  | (KW_STDEV) => "stdDeviation"
  | (KW_MEDIAN) => "median"
  | (KW_CORR) => "correlation"
  | (KW_MEAN) => "mean"
  | (KW_TOSTRING) => "toString"
  | (KW_END) => "end"
  | (KW_DO) => "do"
  | (KW_WHILE) => "while"
  | (KW_ELSE) => "else"
  | (KW_THEN) => "then"
  | (KW_IF) => "if"
  | (KW_GETS) => "getString"
  | (EMPTY) => "{}"
  | (KW_PROD) => "prod"
  | (KW_SUM) => "sum"
  | (KW_terminate) => "terminate"
  | (KW_endvars) => "end variables"
  | (KW_Print) => "print"
  | (STR(_)) => "STR"
  | (KW_commands) => "commands"
  | (DOTDOT) => ":="
  | (TIPO(_)) => "TIPO"
  | (SEMI) => "SEMI"
  | (KW_variables) => "variables"
  | (NEQ) => "!="
  | (GEQ) => ">="
  | (LEQ) => "<="
  | (LT) => "<"
  | (GT) => ">"
  | (SPACE) => " "
  | (NOT) => "!"
  | (OR) => "||"
  | (AND) => "&&"
  | (BOOL(_)) => "BOOL"
  | (RP) => ")"
  | (LP) => "("
  | (SBOOL(_)) => "SBOOL"
  | (COMMA) => ","
  | (MINUS) => "-"
  | (DIV) => "/"
  | (TIMES) => "*"
  | (SFLOAT(_)) => "SFLOAT"
  | (EEQ) => "=="
  | (DOT) => "."
  | (PLUS) => "+"
  | (EQ) => "="
  | (SINT(_)) => "SINT"
  | (REAL(_)) => "REAL"
  | (NUM(_)) => "NUM"
  | (ID(_)) => "ID"
  | (SSTRING(_)) => "SSTRING"
  | (KW_title) => "title"
  | (KW_in) => "in"
  | (KW_let) => "let"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (CONCAT) => false
  | (KW_TOINT) => true
  | (KW_TOFLOAT) => true
  | (KW_GETI) => true
  | (VOID) => false
  | (KW_LINREG) => true
  | (KW_SUBS) => true
  | (KW_COV) => true
  | (KW_GETF) => true
  | (KW_POW) => true
  | (KW_RT) => true
  | (KW_VAR) => true
  | (KW_STDEV) => true
  | (KW_MEDIAN) => true
  | (KW_CORR) => true
  | (KW_MEAN) => true
  | (KW_TOSTRING) => true
  | (KW_END) => true
  | (KW_DO) => true
  | (KW_WHILE) => true
  | (KW_ELSE) => true
  | (KW_THEN) => true
  | (KW_IF) => true
  | (KW_GETS) => true
  | (EMPTY) => false
  | (KW_PROD) => true
  | (KW_SUM) => true
  | (KW_terminate) => true
  | (KW_endvars) => true
  | (KW_Print) => false
  | (STR(_)) => false
  | (KW_commands) => true
  | (DOTDOT) => false
  | (TIPO(_)) => false
  | (SEMI) => false
  | (KW_variables) => true
  | (NEQ) => false
  | (GEQ) => false
  | (LEQ) => false
  | (LT) => false
  | (GT) => false
  | (SPACE) => false
  | (NOT) => false
  | (OR) => false
  | (AND) => false
  | (BOOL(_)) => false
  | (RP) => false
  | (LP) => false
  | (SBOOL(_)) => false
  | (COMMA) => false
  | (MINUS) => false
  | (DIV) => false
  | (TIMES) => false
  | (SFLOAT(_)) => false
  | (EEQ) => false
  | (DOT) => false
  | (PLUS) => false
  | (EQ) => false
  | (SINT(_)) => false
  | (REAL(_)) => false
  | (NUM(_)) => false
  | (ID(_)) => false
  | (SSTRING(_)) => false
  | (KW_title) => true
  | (KW_in) => false
  | (KW_let) => false
(* end case *))


  fun toksToString toks = String.concatWith " " (map toString toks)

  fun isEOF EOF = true
    | isEOF _ = false

end

functor DarwinParseFn(Lex : ANTLR_LEXER) = struct

  local
    structure Tok = 
DarwinTokens
    structure UserCode = struct

 
    open ParseTree

    fun getInt x = (TypeChecker.extractInt x)
    fun getFloat x = (TypeChecker.extractFloat x)
    fun getString x = (TypeChecker.extractString x)
    fun getBool x = (TypeChecker.extractBool x)
    fun getList x = (TypeChecker.extractList x)

    fun getVar v = AtomMap.appi (fn (k,w) => print (
        let val _ = print(Atom.toString k)
            val _ = print (TypeChecker.show w)
        in
            ""
        end)) v


fun program_PROD_1_ACT (SR, STR, SEMI, KW_title, KW_variables, KW_commands, variables, SR_SPAN : (Lex.pos * Lex.pos), STR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_title_SPAN : (Lex.pos * Lex.pos), KW_variables_SPAN : (Lex.pos * Lex.pos), KW_commands_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( )
fun commands_PROD_1_ACT (SEMI, prints, SEMI_SPAN : (Lex.pos * Lex.pos), prints_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( prints)
fun commands_PROD_2_ACT (SEMI, assign, SEMI_SPAN : (Lex.pos * Lex.pos), assign_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( assign)
fun commands_PROD_3_ACT (conditional, conditional_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( conditional)
fun assign_PROD_1_ACT (ID, expr, DOTDOT, ID_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), DOTDOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( tree:=(ParseTree.Assign(ID,expr))::(!tree); (ParseTree.Assign(ID,expr)))
fun expr_PROD_1_ACT (exp_string, exp_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( exp_string)
fun expr_PROD_2_ACT (exp_bool, exp_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( exp_bool)
fun expr_PROD_3_ACT (exp_arit, exp_arit_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( exp_arit)
fun expr_PROD_4_ACT (funcs_string, funcs_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( funcs_string)
fun expr_PROD_5_ACT (funcs_float, funcs_float_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( funcs_float)
fun val_list_PROD_1_ACT (SINT, SINT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.Const (Grammar.Sample (List.map (fn(x) => Grammar.Primitivo(Grammar.Int_ x)) SINT) ))
fun val_list_PROD_2_ACT (SFLOAT, SFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( )
fun val_list_PROD_3_ACT (SBOOL, SBOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( )
fun val_list_PROD_4_ACT (SSTRING, SSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( )
fun val_list_PROD_5_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( )
fun prints_PROD_1_ACT (LP, RP, KW_Print, exp_string, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), exp_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( 
        let
            val k = (ParseTree.Print exp_string)::(!tree)
        in
            tree := k;
            (ParseTree.Print exp_string)
        end
    )
fun funcs_string_PROD_1_ACT (LP, RP, expr, KW_TOSTRING, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), KW_TOSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.FuncOne(ParseTree.ToString,expr))
fun funcs_int_PROD_1_ACT (LP, RP, exp_arit, KW_GETI, COMMA, int_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_arit_SPAN : (Lex.pos * Lex.pos), KW_GETI_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), int_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( )
fun funcs_int_PROD_2_ACT (LP, RP, KW_TOINT, exp_arit, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_TOINT_SPAN : (Lex.pos * Lex.pos), exp_arit_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getFunctionOne(KW_TOINT, exp_arit))
fun funcs_float_PROD_1_ACT (LP, RP, EMPTY, KW_SUM, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), EMPTY_SPAN : (Lex.pos * Lex.pos), KW_SUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getFunctionOne("sum", ParseTree.Const(Grammar.Sample nil)))
fun funcs_float_PROD_2_ACT (LP, RP, float_list, KW_SUM, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), KW_SUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getFunctionOne("sum", float_list))
fun funcs_float_PROD_3_ACT (LP, RP, KW_PROD, EMPTY, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_PROD_SPAN : (Lex.pos * Lex.pos), EMPTY_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getFunctionOne("prod", ParseTree.Const(Grammar.Sample nil)))
fun funcs_float_PROD_4_ACT (LP, RP, KW_PROD, float_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_PROD_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getFunctionOne("prod", float_list))
fun funcs_float_PROD_5_ACT (LP, RP, KW_MEAN, float_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_MEAN_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getFunctionOne("mean", float_list))
fun funcs_float_PROD_6_ACT (LP, RP, KW_CORR, COMMA, float_list1, float_list2, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_CORR_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), float_list1_SPAN : (Lex.pos * Lex.pos), float_list2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getBinaryFun("correlation", float_list1, float_list2))
fun funcs_float_PROD_7_ACT (LP, RP, KW_STDEV, float_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_STDEV_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getFunctionOne("stdDeviation", float_list))
fun funcs_float_PROD_8_ACT (LP, RP, KW_MEDIAN, float_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_MEDIAN_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getFunctionOne("median", float_list))
fun funcs_float_PROD_9_ACT (LP, RP, float_list, KW_VAR, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), KW_VAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getFunctionOne("variance", float_list))
fun funcs_float_PROD_10_ACT (LP, RP, COMMA, float_list1, float_list2, KW_COV, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), float_list1_SPAN : (Lex.pos * Lex.pos), float_list2_SPAN : (Lex.pos * Lex.pos), KW_COV_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getBinaryFun("covariance", float_list1, float_list2))
fun funcs_float_PROD_11_ACT (LP, RP, exp_arit, KW_TOFLOAT, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_arit_SPAN : (Lex.pos * Lex.pos), KW_TOFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getFunctionOne("toFloat", exp_arit))
fun funcs_float_PROD_12_ACT (LP, RP, KW_LINREG, COMMA, float_list1, float_list2, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_LINREG_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), float_list1_SPAN : (Lex.pos * Lex.pos), float_list2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getBinaryFun("linearRegression", float_list1, float_list2))
fun funcs_list_PROD_1_ACT (LP, RP, exp_arit1, exp_arit2, KW_SUBS, val_list, COMMA1, COMMA2, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_arit1_SPAN : (Lex.pos * Lex.pos), exp_arit2_SPAN : (Lex.pos * Lex.pos), KW_SUBS_SPAN : (Lex.pos * Lex.pos), val_list_SPAN : (Lex.pos * Lex.pos), COMMA1_SPAN : (Lex.pos * Lex.pos), COMMA2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( )
fun float_list_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.Var ID)
fun float_list_PROD_2_ACT (SFLOAT, SFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.floatListToSampleExpr(SFLOAT))
fun string_list_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( )
fun string_list_PROD_2_ACT (SSTRING, SSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( )
fun int_list_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( )
fun int_list_PROD_2_ACT (SINT, SINT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( )
fun exp_bool_PROD_1_ACT (rel_op, addExp1, addExp2, rel_op_SPAN : (Lex.pos * Lex.pos), addExp1_SPAN : (Lex.pos * Lex.pos), addExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.getExprBoolTree(rel_op,addExp1,addExp2))
fun exp_bool_PROD_2_ACT (op_bool, op_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( op_bool)
fun exp_bool_PROD_3_ACT (atom_bool, atom_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( atom_bool)
fun op_bool_PROD_1_ACT (AND, atom_bool1, atom_bool2, AND_SPAN : (Lex.pos * Lex.pos), atom_bool1_SPAN : (Lex.pos * Lex.pos), atom_bool2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.FuncTwo(ParseTree.And,atom_bool1,atom_bool2))
fun op_bool_PROD_2_ACT (OR, atom_bool1, atom_bool2, OR_SPAN : (Lex.pos * Lex.pos), atom_bool1_SPAN : (Lex.pos * Lex.pos), atom_bool2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.FuncTwo(ParseTree.Or,atom_bool1,atom_bool2))
fun exp_string_PROD_1_ACT (op_str, op_str_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( op_str)
fun exp_string_PROD_2_ACT (atom_string, atom_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( atom_string)
fun op_str_PROD_1_ACT (atom_string1, atom_string2, CONCAT, atom_string1_SPAN : (Lex.pos * Lex.pos), atom_string2_SPAN : (Lex.pos * Lex.pos), CONCAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.FuncTwo(ParseTree.Concat,atom_string1,atom_string2))
fun atom_string_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.Var ID)
fun atom_string_PROD_2_ACT (STR, STR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.Const (Grammar.Primitivo (Grammar.String_ STR)))
fun rel_op_PROD_1_ACT (EEQ, EEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( "==")
fun rel_op_PROD_2_ACT (NEQ, NEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( "!=")
fun rel_op_PROD_3_ACT (GEQ, GEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ">=")
fun rel_op_PROD_4_ACT (LEQ, LEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( "<=")
fun rel_op_PROD_5_ACT (LT, LT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( "<")
fun rel_op_PROD_6_ACT (GT, GT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ">")
fun atom_bool_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.Var ID)
fun atom_bool_PROD_2_ACT (BOOL, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  (  (ParseTree.Const (Grammar.Primitivo (Grammar.Boolean_ BOOL))))
fun loop_PROD_1_ACT (commands, KW_WHILE, exp_bool, KW_DO, KW_END, commands_SPAN : (Lex.pos * Lex.pos), KW_WHILE_SPAN : (Lex.pos * Lex.pos), exp_bool_SPAN : (Lex.pos * Lex.pos), KW_DO_SPAN : (Lex.pos * Lex.pos), KW_END_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( )
fun conditional_PROD_1_ACT (SR1, SR2, exp_bool, KW_ELSE, KW_THEN, KW_IF, KW_END, SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), exp_bool_SPAN : (Lex.pos * Lex.pos), KW_ELSE_SPAN : (Lex.pos * Lex.pos), KW_THEN_SPAN : (Lex.pos * Lex.pos), KW_IF_SPAN : (Lex.pos * Lex.pos), KW_END_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( 
            let
                val ifi = (ParseTree.If(exp_bool,SR1,SR2))
            in
                tree := (ifi :: (!tree));
                ifi
            end
           )
fun exp_arit_PROD_1_ACT (addExp, addExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( addExp)
fun exp_arit_PROD_2_ACT (atomicExp, atomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( atomicExp)
fun addExp_PROD_1_ACT (PLUS, multExp1, multExp2, PLUS_SPAN : (Lex.pos * Lex.pos), multExp1_SPAN : (Lex.pos * Lex.pos), multExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.FuncTwo(ParseTree.Add,multExp1,multExp2))
fun addExp_PROD_2_ACT (multExp1, multExp2, MINUS, multExp1_SPAN : (Lex.pos * Lex.pos), multExp2_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.FuncTwo(ParseTree.Sub,multExp1,multExp2))
fun addExp_PROD_3_ACT (multExp, multExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( multExp)
fun multExp_PROD_1_ACT (DIV, powExp1, powExp2, DIV_SPAN : (Lex.pos * Lex.pos), powExp1_SPAN : (Lex.pos * Lex.pos), powExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.FuncTwo(ParseTree.Div,powExp1,powExp2))
fun multExp_PROD_2_ACT (TIMES, powExp1, powExp2, TIMES_SPAN : (Lex.pos * Lex.pos), powExp1_SPAN : (Lex.pos * Lex.pos), powExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.FuncTwo(ParseTree.Mul,powExp1,powExp2))
fun multExp_PROD_3_ACT (powExp, powExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( powExp)
fun powExp_PROD_1_ACT (LP, RP, COMMA, prefixExp1, prefixExp2, KW_POW, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), prefixExp1_SPAN : (Lex.pos * Lex.pos), prefixExp2_SPAN : (Lex.pos * Lex.pos), KW_POW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.FuncTwo(ParseTree.Pow,prefixExp1,prefixExp2))
fun powExp_PROD_2_ACT (LP, RP, COMMA, KW_RT, prefixExp1, prefixExp2, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), KW_RT_SPAN : (Lex.pos * Lex.pos), prefixExp1_SPAN : (Lex.pos * Lex.pos), prefixExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.FuncTwo(ParseTree.RT,prefixExp1,prefixExp2))
fun powExp_PROD_3_ACT (atomicExp, atomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( atomicExp)
fun prefixExp_PROD_1_ACT (atomicExp, atomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( atomicExp)
fun prefixExp_PROD_2_ACT (MINUS, prefixExp, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.FuncTwo(ParseTree.Sub,ParseTree.Const(Grammar.Primitivo (Grammar.Int_ 0)),prefixExp))
fun atomicExp_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.Var ID)
fun atomicExp_PROD_2_ACT (NUM, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.Const (Grammar.Primitivo (Grammar.Int_ NUM)))
fun atomicExp_PROD_3_ACT (REAL, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( ParseTree.Const (Grammar.Primitivo (Grammar.Float_ REAL)))
fun variables_PROD_1_ACT (SR, KW_endvars, SR_SPAN : (Lex.pos * Lex.pos), KW_endvars_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( (fn(_) => ()) SR)
fun declaration_PROD_1_ACT (ID, SEMI, TIPO, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TIPO_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts, tree, vars) = 
  ( vars:=ParseTree.insere(!vars,Atom.atom ID,Atom.toString(Atom.atom TIPO));
                      ts:=AtomMap.insert(!ts,Atom.atom ID,TIPO))
fun mkts_REFC() : (string AtomMap.map) ref = ref ( AtomMap.empty)
fun mktree_REFC() : (ParseTree.RoseTree) ref = ref ( nil)
fun mkvars_REFC() : (tipo AtomMap.map) ref = ref ( AtomMap.empty)

    end

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)
    structure EBNF = AntlrEBNF(struct
			         type strm = Err.wstream
			         val getSpan = Err.getSpan
			       end)

    fun mk lexFn = let
val ts_REFC = UserCode.mkts_REFC()
val tree_REFC = UserCode.mktree_REFC()
val vars_REFC = UserCode.mkvars_REFC()
fun getS() = {ts = !ts_REFC, tree = !tree_REFC, vars = !vars_REFC}
fun putS{ts, tree, vars} = (ts_REFC := ts; tree_REFC := tree; vars_REFC := vars)
fun unwrap (ret, strm, repairs) = (ret, strm, repairs, getS())
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) = 
	        (Err.whileDisabled eh (fn() => prod strm)) 
		handle Err.ParseError => try (prods)
          in try prods end
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCONCAT strm = (case (lex(strm))
 of (Tok.CONCAT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_TOINT strm = (case (lex(strm))
 of (Tok.KW_TOINT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_TOFLOAT strm = (case (lex(strm))
 of (Tok.KW_TOFLOAT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_GETI strm = (case (lex(strm))
 of (Tok.KW_GETI, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchVOID strm = (case (lex(strm))
 of (Tok.VOID, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_LINREG strm = (case (lex(strm))
 of (Tok.KW_LINREG, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_SUBS strm = (case (lex(strm))
 of (Tok.KW_SUBS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_COV strm = (case (lex(strm))
 of (Tok.KW_COV, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_GETF strm = (case (lex(strm))
 of (Tok.KW_GETF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_POW strm = (case (lex(strm))
 of (Tok.KW_POW, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_RT strm = (case (lex(strm))
 of (Tok.KW_RT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_VAR strm = (case (lex(strm))
 of (Tok.KW_VAR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_STDEV strm = (case (lex(strm))
 of (Tok.KW_STDEV, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_MEDIAN strm = (case (lex(strm))
 of (Tok.KW_MEDIAN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_CORR strm = (case (lex(strm))
 of (Tok.KW_CORR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_MEAN strm = (case (lex(strm))
 of (Tok.KW_MEAN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_TOSTRING strm = (case (lex(strm))
 of (Tok.KW_TOSTRING, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_END strm = (case (lex(strm))
 of (Tok.KW_END, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_DO strm = (case (lex(strm))
 of (Tok.KW_DO, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_WHILE strm = (case (lex(strm))
 of (Tok.KW_WHILE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_ELSE strm = (case (lex(strm))
 of (Tok.KW_ELSE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_THEN strm = (case (lex(strm))
 of (Tok.KW_THEN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_IF strm = (case (lex(strm))
 of (Tok.KW_IF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_GETS strm = (case (lex(strm))
 of (Tok.KW_GETS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEMPTY strm = (case (lex(strm))
 of (Tok.EMPTY, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_PROD strm = (case (lex(strm))
 of (Tok.KW_PROD, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_SUM strm = (case (lex(strm))
 of (Tok.KW_SUM, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_terminate strm = (case (lex(strm))
 of (Tok.KW_terminate, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_endvars strm = (case (lex(strm))
 of (Tok.KW_endvars, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_Print strm = (case (lex(strm))
 of (Tok.KW_Print, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSTR strm = (case (lex(strm))
 of (Tok.STR(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchKW_commands strm = (case (lex(strm))
 of (Tok.KW_commands, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDOTDOT strm = (case (lex(strm))
 of (Tok.DOTDOT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTIPO strm = (case (lex(strm))
 of (Tok.TIPO(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchSEMI strm = (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_variables strm = (case (lex(strm))
 of (Tok.KW_variables, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNEQ strm = (case (lex(strm))
 of (Tok.NEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchGEQ strm = (case (lex(strm))
 of (Tok.GEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLEQ strm = (case (lex(strm))
 of (Tok.LEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLT strm = (case (lex(strm))
 of (Tok.LT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchGT strm = (case (lex(strm))
 of (Tok.GT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSPACE strm = (case (lex(strm))
 of (Tok.SPACE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNOT strm = (case (lex(strm))
 of (Tok.NOT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOR strm = (case (lex(strm))
 of (Tok.OR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchAND strm = (case (lex(strm))
 of (Tok.AND, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBOOL strm = (case (lex(strm))
 of (Tok.BOOL(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchRP strm = (case (lex(strm))
 of (Tok.RP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLP strm = (case (lex(strm))
 of (Tok.LP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSBOOL strm = (case (lex(strm))
 of (Tok.SBOOL(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMA strm = (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMINUS strm = (case (lex(strm))
 of (Tok.MINUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDIV strm = (case (lex(strm))
 of (Tok.DIV, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTIMES strm = (case (lex(strm))
 of (Tok.TIMES, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSFLOAT strm = (case (lex(strm))
 of (Tok.SFLOAT(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchEEQ strm = (case (lex(strm))
 of (Tok.EEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDOT strm = (case (lex(strm))
 of (Tok.DOT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPLUS strm = (case (lex(strm))
 of (Tok.PLUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEQ strm = (case (lex(strm))
 of (Tok.EQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSINT strm = (case (lex(strm))
 of (Tok.SINT(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchREAL strm = (case (lex(strm))
 of (Tok.REAL(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchNUM strm = (case (lex(strm))
 of (Tok.NUM(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchID strm = (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchSSTRING strm = (case (lex(strm))
 of (Tok.SSTRING(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchKW_title strm = (case (lex(strm))
 of (Tok.KW_title, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_in strm = (case (lex(strm))
 of (Tok.KW_in, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_let strm = (case (lex(strm))
 of (Tok.KW_let, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (program_NT) = 
let
fun addExp_NT (strm) = let
      fun addExp_PROD_1 (strm) = let
            val (multExp1_RES, multExp1_SPAN, strm') = multExp_NT(strm)
            val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm')
            val (multExp2_RES, multExp2_SPAN, strm') = multExp_NT(strm')
            val FULL_SPAN = (#1(multExp1_SPAN), #2(multExp2_SPAN))
            in
              (UserCode.addExp_PROD_1_ACT (PLUS_RES, multExp1_RES, multExp2_RES, PLUS_SPAN : (Lex.pos * Lex.pos), multExp1_SPAN : (Lex.pos * Lex.pos), multExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun addExp_PROD_2 (strm) = let
            val (multExp1_RES, multExp1_SPAN, strm') = multExp_NT(strm)
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm')
            val (multExp2_RES, multExp2_SPAN, strm') = multExp_NT(strm')
            val FULL_SPAN = (#1(multExp1_SPAN), #2(multExp2_SPAN))
            in
              (UserCode.addExp_PROD_2_ACT (multExp1_RES, multExp2_RES, MINUS_RES, multExp1_SPAN : (Lex.pos * Lex.pos), multExp2_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun addExp_PROD_3 (strm) = let
            val (multExp_RES, multExp_SPAN, strm') = multExp_NT(strm)
            val FULL_SPAN = (#1(multExp_SPAN), #2(multExp_SPAN))
            in
              (UserCode.addExp_PROD_3_ACT (multExp_RES, multExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.NUM(_), _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.REAL(_), _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_RT, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.KW_POW, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | _ => fail()
        (* end case *))
      end
and multExp_NT (strm) = let
      fun multExp_PROD_1 (strm) = let
            val (powExp1_RES, powExp1_SPAN, strm') = powExp_NT(strm)
            val (DIV_RES, DIV_SPAN, strm') = matchDIV(strm')
            val (powExp2_RES, powExp2_SPAN, strm') = powExp_NT(strm')
            val FULL_SPAN = (#1(powExp1_SPAN), #2(powExp2_SPAN))
            in
              (UserCode.multExp_PROD_1_ACT (DIV_RES, powExp1_RES, powExp2_RES, DIV_SPAN : (Lex.pos * Lex.pos), powExp1_SPAN : (Lex.pos * Lex.pos), powExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun multExp_PROD_2 (strm) = let
            val (powExp1_RES, powExp1_SPAN, strm') = powExp_NT(strm)
            val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm')
            val (powExp2_RES, powExp2_SPAN, strm') = powExp_NT(strm')
            val FULL_SPAN = (#1(powExp1_SPAN), #2(powExp2_SPAN))
            in
              (UserCode.multExp_PROD_2_ACT (TIMES_RES, powExp1_RES, powExp2_RES, TIMES_SPAN : (Lex.pos * Lex.pos), powExp1_SPAN : (Lex.pos * Lex.pos), powExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun multExp_PROD_3 (strm) = let
            val (powExp_RES, powExp_SPAN, strm') = powExp_NT(strm)
            val FULL_SPAN = (#1(powExp_SPAN), #2(powExp_SPAN))
            in
              (UserCode.multExp_PROD_3_ACT (powExp_RES, powExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.NUM(_), _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.REAL(_), _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_RT, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | (Tok.KW_POW, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2, multExp_PROD_3])
          | _ => fail()
        (* end case *))
      end
and powExp_NT (strm) = let
      fun powExp_PROD_1 (strm) = let
            val (KW_POW_RES, KW_POW_SPAN, strm') = matchKW_POW(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (prefixExp1_RES, prefixExp1_SPAN, strm') = prefixExp_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (prefixExp2_RES, prefixExp2_SPAN, strm') = prefixExp_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_POW_SPAN), #2(RP_SPAN))
            in
              (UserCode.powExp_PROD_1_ACT (LP_RES, RP_RES, COMMA_RES, prefixExp1_RES, prefixExp2_RES, KW_POW_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), prefixExp1_SPAN : (Lex.pos * Lex.pos), prefixExp2_SPAN : (Lex.pos * Lex.pos), KW_POW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun powExp_PROD_2 (strm) = let
            val (KW_RT_RES, KW_RT_SPAN, strm') = matchKW_RT(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (prefixExp1_RES, prefixExp1_SPAN, strm') = prefixExp_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (prefixExp2_RES, prefixExp2_SPAN, strm') = prefixExp_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_RT_SPAN), #2(RP_SPAN))
            in
              (UserCode.powExp_PROD_2_ACT (LP_RES, RP_RES, COMMA_RES, KW_RT_RES, prefixExp1_RES, prefixExp2_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), KW_RT_SPAN : (Lex.pos * Lex.pos), prefixExp1_SPAN : (Lex.pos * Lex.pos), prefixExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun powExp_PROD_3 (strm) = let
            val (atomicExp_RES, atomicExp_SPAN, strm') = atomicExp_NT(strm)
            val FULL_SPAN = (#1(atomicExp_SPAN), #2(atomicExp_SPAN))
            in
              (UserCode.powExp_PROD_3_ACT (atomicExp_RES, atomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => powExp_PROD_3(strm)
          | (Tok.NUM(_), _, strm') => powExp_PROD_3(strm)
          | (Tok.REAL(_), _, strm') => powExp_PROD_3(strm)
          | (Tok.LP, _, strm') => powExp_PROD_3(strm)
          | (Tok.KW_POW, _, strm') => powExp_PROD_1(strm)
          | (Tok.KW_RT, _, strm') => powExp_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and atomicExp_NT (strm) = let
      fun atomicExp_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.atomicExp_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atomicExp_PROD_2 (strm) = let
            val (NUM_RES, NUM_SPAN, strm') = matchNUM(strm)
            val FULL_SPAN = (#1(NUM_SPAN), #2(NUM_SPAN))
            in
              (UserCode.atomicExp_PROD_2_ACT (NUM_RES, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atomicExp_PROD_3 (strm) = let
            val (REAL_RES, REAL_SPAN, strm') = matchREAL(strm)
            val FULL_SPAN = (#1(REAL_SPAN), #2(REAL_SPAN))
            in
              (UserCode.atomicExp_PROD_3_ACT (REAL_RES, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atomicExp_PROD_4 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_arit_RES, exp_arit_SPAN, strm') = exp_arit_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              ((exp_arit_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => atomicExp_PROD_4(strm)
          | (Tok.NUM(_), _, strm') => atomicExp_PROD_2(strm)
          | (Tok.ID(_), _, strm') => atomicExp_PROD_1(strm)
          | (Tok.REAL(_), _, strm') => atomicExp_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
and exp_arit_NT (strm) = let
      fun exp_arit_PROD_1 (strm) = let
            val (addExp_RES, addExp_SPAN, strm') = addExp_NT(strm)
            val FULL_SPAN = (#1(addExp_SPAN), #2(addExp_SPAN))
            in
              (UserCode.exp_arit_PROD_1_ACT (addExp_RES, addExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun exp_arit_PROD_2 (strm) = let
            val (atomicExp_RES, atomicExp_SPAN, strm') = atomicExp_NT(strm)
            val FULL_SPAN = (#1(atomicExp_SPAN), #2(atomicExp_SPAN))
            in
              (UserCode.exp_arit_PROD_2_ACT (atomicExp_RES, atomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.NUM(_), _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.REAL(_), _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [exp_arit_PROD_1, exp_arit_PROD_2])
          | (Tok.KW_RT, _, strm') => exp_arit_PROD_1(strm)
          | (Tok.KW_POW, _, strm') => exp_arit_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and prefixExp_NT (strm) = let
      fun prefixExp_PROD_1 (strm) = let
            val (atomicExp_RES, atomicExp_SPAN, strm') = atomicExp_NT(strm)
            val FULL_SPAN = (#1(atomicExp_SPAN), #2(atomicExp_SPAN))
            in
              (UserCode.prefixExp_PROD_1_ACT (atomicExp_RES, atomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun prefixExp_PROD_2 (strm) = let
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
            val (prefixExp_RES, prefixExp_SPAN, strm') = prefixExp_NT(strm')
            val FULL_SPAN = (#1(MINUS_SPAN), #2(prefixExp_SPAN))
            in
              (UserCode.prefixExp_PROD_2_ACT (MINUS_RES, prefixExp_RES, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.MINUS, _, strm') => prefixExp_PROD_2(strm)
          | (Tok.ID(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.NUM(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.REAL(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.LP, _, strm') => prefixExp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun rel_op_NT (strm) = let
      fun rel_op_PROD_1 (strm) = let
            val (EEQ_RES, EEQ_SPAN, strm') = matchEEQ(strm)
            val FULL_SPAN = (#1(EEQ_SPAN), #2(EEQ_SPAN))
            in
              (UserCode.rel_op_PROD_1_ACT (EEQ_RES, EEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun rel_op_PROD_2 (strm) = let
            val (NEQ_RES, NEQ_SPAN, strm') = matchNEQ(strm)
            val FULL_SPAN = (#1(NEQ_SPAN), #2(NEQ_SPAN))
            in
              (UserCode.rel_op_PROD_2_ACT (NEQ_RES, NEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun rel_op_PROD_3 (strm) = let
            val (GEQ_RES, GEQ_SPAN, strm') = matchGEQ(strm)
            val FULL_SPAN = (#1(GEQ_SPAN), #2(GEQ_SPAN))
            in
              (UserCode.rel_op_PROD_3_ACT (GEQ_RES, GEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun rel_op_PROD_4 (strm) = let
            val (LEQ_RES, LEQ_SPAN, strm') = matchLEQ(strm)
            val FULL_SPAN = (#1(LEQ_SPAN), #2(LEQ_SPAN))
            in
              (UserCode.rel_op_PROD_4_ACT (LEQ_RES, LEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun rel_op_PROD_5 (strm) = let
            val (LT_RES, LT_SPAN, strm') = matchLT(strm)
            val FULL_SPAN = (#1(LT_SPAN), #2(LT_SPAN))
            in
              (UserCode.rel_op_PROD_5_ACT (LT_RES, LT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun rel_op_PROD_6 (strm) = let
            val (GT_RES, GT_SPAN, strm') = matchGT(strm)
            val FULL_SPAN = (#1(GT_SPAN), #2(GT_SPAN))
            in
              (UserCode.rel_op_PROD_6_ACT (GT_RES, GT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.GT, _, strm') => rel_op_PROD_6(strm)
          | (Tok.LEQ, _, strm') => rel_op_PROD_4(strm)
          | (Tok.NEQ, _, strm') => rel_op_PROD_2(strm)
          | (Tok.EEQ, _, strm') => rel_op_PROD_1(strm)
          | (Tok.GEQ, _, strm') => rel_op_PROD_3(strm)
          | (Tok.LT, _, strm') => rel_op_PROD_5(strm)
          | _ => fail()
        (* end case *))
      end
fun exp_bool_NT (strm) = let
      fun exp_bool_PROD_1 (strm) = let
            val (addExp1_RES, addExp1_SPAN, strm') = addExp_NT(strm)
            val (rel_op_RES, rel_op_SPAN, strm') = rel_op_NT(strm')
            val (addExp2_RES, addExp2_SPAN, strm') = addExp_NT(strm')
            val FULL_SPAN = (#1(addExp1_SPAN), #2(addExp2_SPAN))
            in
              (UserCode.exp_bool_PROD_1_ACT (rel_op_RES, addExp1_RES, addExp2_RES, rel_op_SPAN : (Lex.pos * Lex.pos), addExp1_SPAN : (Lex.pos * Lex.pos), addExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun exp_bool_PROD_2 (strm) = let
            val (op_bool_RES, op_bool_SPAN, strm') = op_bool_NT(strm)
            val FULL_SPAN = (#1(op_bool_SPAN), #2(op_bool_SPAN))
            in
              (UserCode.exp_bool_PROD_2_ACT (op_bool_RES, op_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun exp_bool_PROD_3 (strm) = let
            val (atom_bool_RES, atom_bool_SPAN, strm') = atom_bool_NT(strm)
            val FULL_SPAN = (#1(atom_bool_SPAN), #2(atom_bool_SPAN))
            in
              (UserCode.exp_bool_PROD_3_ACT (atom_bool_RES, atom_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.NUM(_), _, strm') => exp_bool_PROD_1(strm)
          | (Tok.REAL(_), _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_RT, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.KW_POW, _, strm') => exp_bool_PROD_1(strm)
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [exp_bool_PROD_1, exp_bool_PROD_2,
                exp_bool_PROD_3])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [exp_bool_PROD_1, exp_bool_PROD_2,
                exp_bool_PROD_3])
          | (Tok.BOOL(_), _, strm') =>
              tryProds(strm, [exp_bool_PROD_2, exp_bool_PROD_3])
          | _ => fail()
        (* end case *))
      end
and atom_bool_NT (strm) = let
      fun atom_bool_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.atom_bool_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atom_bool_PROD_2 (strm) = let
            val (BOOL_RES, BOOL_SPAN, strm') = matchBOOL(strm)
            val FULL_SPAN = (#1(BOOL_SPAN), #2(BOOL_SPAN))
            in
              (UserCode.atom_bool_PROD_2_ACT (BOOL_RES, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atom_bool_PROD_3 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_bool_RES, exp_bool_SPAN, strm') = exp_bool_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              ((exp_bool_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => atom_bool_PROD_3(strm)
          | (Tok.ID(_), _, strm') => atom_bool_PROD_1(strm)
          | (Tok.BOOL(_), _, strm') => atom_bool_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and op_bool_NT (strm) = let
      fun op_bool_PROD_1 (strm) = let
            val (atom_bool1_RES, atom_bool1_SPAN, strm') = atom_bool_NT(strm)
            val (AND_RES, AND_SPAN, strm') = matchAND(strm')
            val (atom_bool2_RES, atom_bool2_SPAN, strm') = atom_bool_NT(strm')
            val FULL_SPAN = (#1(atom_bool1_SPAN), #2(atom_bool2_SPAN))
            in
              (UserCode.op_bool_PROD_1_ACT (AND_RES, atom_bool1_RES, atom_bool2_RES, AND_SPAN : (Lex.pos * Lex.pos), atom_bool1_SPAN : (Lex.pos * Lex.pos), atom_bool2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun op_bool_PROD_2 (strm) = let
            val (atom_bool1_RES, atom_bool1_SPAN, strm') = atom_bool_NT(strm)
            val (OR_RES, OR_SPAN, strm') = matchOR(strm')
            val (atom_bool2_RES, atom_bool2_SPAN, strm') = atom_bool_NT(strm')
            val FULL_SPAN = (#1(atom_bool1_SPAN), #2(atom_bool2_SPAN))
            in
              (UserCode.op_bool_PROD_2_ACT (OR_RES, atom_bool1_RES, atom_bool2_RES, OR_SPAN : (Lex.pos * Lex.pos), atom_bool1_SPAN : (Lex.pos * Lex.pos), atom_bool2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              tryProds(strm, [op_bool_PROD_1, op_bool_PROD_2])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [op_bool_PROD_1, op_bool_PROD_2])
          | (Tok.BOOL(_), _, strm') =>
              tryProds(strm, [op_bool_PROD_1, op_bool_PROD_2])
          | _ => fail()
        (* end case *))
      end
fun float_list_NT (strm) = let
      fun float_list_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.float_list_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun float_list_PROD_2 (strm) = let
            val (SFLOAT_RES, SFLOAT_SPAN, strm') = matchSFLOAT(strm)
            val FULL_SPAN = (#1(SFLOAT_SPAN), #2(SFLOAT_SPAN))
            in
              (UserCode.float_list_PROD_2_ACT (SFLOAT_RES, SFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.SFLOAT(_), _, strm') => float_list_PROD_2(strm)
          | (Tok.ID(_), _, strm') => float_list_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun funcs_float_NT (strm) = let
      fun funcs_float_PROD_1 (strm) = let
            val (KW_SUM_RES, KW_SUM_SPAN, strm') = matchKW_SUM(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (EMPTY_RES, EMPTY_SPAN, strm') = matchEMPTY(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_SUM_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_1_ACT (LP_RES, RP_RES, EMPTY_RES, KW_SUM_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), EMPTY_SPAN : (Lex.pos * Lex.pos), KW_SUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_2 (strm) = let
            val (KW_SUM_RES, KW_SUM_SPAN, strm') = matchKW_SUM(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list_RES, float_list_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_SUM_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_2_ACT (LP_RES, RP_RES, float_list_RES, KW_SUM_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), KW_SUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_3 (strm) = let
            val (KW_PROD_RES, KW_PROD_SPAN, strm') = matchKW_PROD(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (EMPTY_RES, EMPTY_SPAN, strm') = matchEMPTY(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_PROD_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_3_ACT (LP_RES, RP_RES, KW_PROD_RES, EMPTY_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_PROD_SPAN : (Lex.pos * Lex.pos), EMPTY_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_4 (strm) = let
            val (KW_PROD_RES, KW_PROD_SPAN, strm') = matchKW_PROD(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list_RES, float_list_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_PROD_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_4_ACT (LP_RES, RP_RES, KW_PROD_RES, float_list_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_PROD_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_5 (strm) = let
            val (KW_MEAN_RES, KW_MEAN_SPAN, strm') = matchKW_MEAN(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list_RES, float_list_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_MEAN_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_5_ACT (LP_RES, RP_RES, KW_MEAN_RES, float_list_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_MEAN_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_6 (strm) = let
            val (KW_CORR_RES, KW_CORR_SPAN, strm') = matchKW_CORR(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list1_RES, float_list1_SPAN, strm') = float_list_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (float_list2_RES, float_list2_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_CORR_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_6_ACT (LP_RES, RP_RES, KW_CORR_RES, COMMA_RES, float_list1_RES, float_list2_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_CORR_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), float_list1_SPAN : (Lex.pos * Lex.pos), float_list2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_7 (strm) = let
            val (KW_STDEV_RES, KW_STDEV_SPAN, strm') = matchKW_STDEV(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list_RES, float_list_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_STDEV_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_7_ACT (LP_RES, RP_RES, KW_STDEV_RES, float_list_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_STDEV_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_8 (strm) = let
            val (KW_MEDIAN_RES, KW_MEDIAN_SPAN, strm') = matchKW_MEDIAN(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list_RES, float_list_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_MEDIAN_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_8_ACT (LP_RES, RP_RES, KW_MEDIAN_RES, float_list_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_MEDIAN_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_9 (strm) = let
            val (KW_VAR_RES, KW_VAR_SPAN, strm') = matchKW_VAR(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list_RES, float_list_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_VAR_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_9_ACT (LP_RES, RP_RES, float_list_RES, KW_VAR_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), KW_VAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_10 (strm) = let
            val (KW_COV_RES, KW_COV_SPAN, strm') = matchKW_COV(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list1_RES, float_list1_SPAN, strm') = float_list_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (float_list2_RES, float_list2_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_COV_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_10_ACT (LP_RES, RP_RES, COMMA_RES, float_list1_RES, float_list2_RES, KW_COV_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), float_list1_SPAN : (Lex.pos * Lex.pos), float_list2_SPAN : (Lex.pos * Lex.pos), KW_COV_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_11 (strm) = let
            val (KW_TOFLOAT_RES, KW_TOFLOAT_SPAN, strm') = matchKW_TOFLOAT(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (exp_arit_RES, exp_arit_SPAN, strm') = exp_arit_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_TOFLOAT_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_11_ACT (LP_RES, RP_RES, exp_arit_RES, KW_TOFLOAT_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_arit_SPAN : (Lex.pos * Lex.pos), KW_TOFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_12 (strm) = let
            val (KW_LINREG_RES, KW_LINREG_SPAN, strm') = matchKW_LINREG(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list1_RES, float_list1_SPAN, strm') = float_list_NT(strm')
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm')
            val (float_list2_RES, float_list2_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_LINREG_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_12_ACT (LP_RES, RP_RES, KW_LINREG_RES, COMMA_RES, float_list1_RES, float_list2_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_LINREG_SPAN : (Lex.pos * Lex.pos), COMMA_SPAN : (Lex.pos * Lex.pos), float_list1_SPAN : (Lex.pos * Lex.pos), float_list2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_LINREG, _, strm') => funcs_float_PROD_12(strm)
          | (Tok.KW_COV, _, strm') => funcs_float_PROD_10(strm)
          | (Tok.KW_MEDIAN, _, strm') => funcs_float_PROD_8(strm)
          | (Tok.KW_CORR, _, strm') => funcs_float_PROD_6(strm)
          | (Tok.KW_SUM, _, strm') =>
              (case (lex(strm'))
               of (Tok.LP, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.ID(_), _, strm') => funcs_float_PROD_2(strm)
                      | (Tok.SFLOAT(_), _, strm') => funcs_float_PROD_2(strm)
                      | (Tok.EMPTY, _, strm') => funcs_float_PROD_1(strm)
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.KW_PROD, _, strm') =>
              (case (lex(strm'))
               of (Tok.LP, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.ID(_), _, strm') => funcs_float_PROD_4(strm)
                      | (Tok.SFLOAT(_), _, strm') => funcs_float_PROD_4(strm)
                      | (Tok.EMPTY, _, strm') => funcs_float_PROD_3(strm)
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.KW_MEAN, _, strm') => funcs_float_PROD_5(strm)
          | (Tok.KW_STDEV, _, strm') => funcs_float_PROD_7(strm)
          | (Tok.KW_VAR, _, strm') => funcs_float_PROD_9(strm)
          | (Tok.KW_TOFLOAT, _, strm') => funcs_float_PROD_11(strm)
          | _ => fail()
        (* end case *))
      end
fun expr_NT (strm) = let
      fun expr_PROD_1 (strm) = let
            val (exp_string_RES, exp_string_SPAN, strm') = exp_string_NT(strm)
            val FULL_SPAN = (#1(exp_string_SPAN), #2(exp_string_SPAN))
            in
              (UserCode.expr_PROD_1_ACT (exp_string_RES, exp_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_2 (strm) = let
            val (exp_bool_RES, exp_bool_SPAN, strm') = exp_bool_NT(strm)
            val FULL_SPAN = (#1(exp_bool_SPAN), #2(exp_bool_SPAN))
            in
              (UserCode.expr_PROD_2_ACT (exp_bool_RES, exp_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_3 (strm) = let
            val (exp_arit_RES, exp_arit_SPAN, strm') = exp_arit_NT(strm)
            val FULL_SPAN = (#1(exp_arit_SPAN), #2(exp_arit_SPAN))
            in
              (UserCode.expr_PROD_3_ACT (exp_arit_RES, exp_arit_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_4 (strm) = let
            val (funcs_string_RES, funcs_string_SPAN, strm') = funcs_string_NT(strm)
            val FULL_SPAN = (#1(funcs_string_SPAN), #2(funcs_string_SPAN))
            in
              (UserCode.expr_PROD_4_ACT (funcs_string_RES, funcs_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_5 (strm) = let
            val (funcs_float_RES, funcs_float_SPAN, strm') = funcs_float_NT(strm)
            val FULL_SPAN = (#1(funcs_float_SPAN), #2(funcs_float_SPAN))
            in
              (UserCode.expr_PROD_5_ACT (funcs_float_RES, funcs_float_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_SUM, _, strm') => expr_PROD_5(strm)
          | (Tok.KW_PROD, _, strm') => expr_PROD_5(strm)
          | (Tok.KW_MEAN, _, strm') => expr_PROD_5(strm)
          | (Tok.KW_CORR, _, strm') => expr_PROD_5(strm)
          | (Tok.KW_MEDIAN, _, strm') => expr_PROD_5(strm)
          | (Tok.KW_STDEV, _, strm') => expr_PROD_5(strm)
          | (Tok.KW_VAR, _, strm') => expr_PROD_5(strm)
          | (Tok.KW_COV, _, strm') => expr_PROD_5(strm)
          | (Tok.KW_LINREG, _, strm') => expr_PROD_5(strm)
          | (Tok.KW_TOFLOAT, _, strm') => expr_PROD_5(strm)
          | (Tok.STR(_), _, strm') => expr_PROD_1(strm)
          | (Tok.KW_TOSTRING, _, strm') =>
              tryProds(strm, [expr_PROD_1, expr_PROD_4])
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [expr_PROD_1, expr_PROD_2, expr_PROD_3])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [expr_PROD_1, expr_PROD_2, expr_PROD_3])
          | (Tok.NUM(_), _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3])
          | (Tok.REAL(_), _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3])
          | (Tok.KW_RT, _, strm') => tryProds(strm, [expr_PROD_2, expr_PROD_3])
          | (Tok.KW_POW, _, strm') =>
              tryProds(strm, [expr_PROD_2, expr_PROD_3])
          | (Tok.BOOL(_), _, strm') => expr_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and funcs_string_NT (strm) = let
      val (KW_TOSTRING_RES, KW_TOSTRING_SPAN, strm') = matchKW_TOSTRING(strm)
      val (LP_RES, LP_SPAN, strm') = matchLP(strm')
      val (expr_RES, expr_SPAN, strm') = expr_NT(strm')
      val (RP_RES, RP_SPAN, strm') = matchRP(strm')
      val FULL_SPAN = (#1(KW_TOSTRING_SPAN), #2(RP_SPAN))
      in
        (UserCode.funcs_string_PROD_1_ACT (LP_RES, RP_RES, expr_RES, KW_TOSTRING_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), KW_TOSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
and exp_string_NT (strm) = let
      fun exp_string_PROD_1 (strm) = let
            val (op_str_RES, op_str_SPAN, strm') = op_str_NT(strm)
            val FULL_SPAN = (#1(op_str_SPAN), #2(op_str_SPAN))
            in
              (UserCode.exp_string_PROD_1_ACT (op_str_RES, op_str_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun exp_string_PROD_2 (strm) = let
            val (atom_string_RES, atom_string_SPAN, strm') = atom_string_NT(strm)
            val FULL_SPAN = (#1(atom_string_SPAN), #2(atom_string_SPAN))
            in
              (UserCode.exp_string_PROD_2_ACT (atom_string_RES, atom_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              tryProds(strm, [exp_string_PROD_1, exp_string_PROD_2])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [exp_string_PROD_1, exp_string_PROD_2])
          | (Tok.STR(_), _, strm') =>
              tryProds(strm, [exp_string_PROD_1, exp_string_PROD_2])
          | (Tok.KW_TOSTRING, _, strm') =>
              tryProds(strm, [exp_string_PROD_1, exp_string_PROD_2])
          | _ => fail()
        (* end case *))
      end
and atom_string_NT (strm) = let
      fun atom_string_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.atom_string_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atom_string_PROD_2 (strm) = let
            val (STR_RES, STR_SPAN, strm') = matchSTR(strm)
            val FULL_SPAN = (#1(STR_SPAN), #2(STR_SPAN))
            in
              (UserCode.atom_string_PROD_2_ACT (STR_RES, STR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atom_string_PROD_3 (strm) = let
            val (funcs_string_RES, funcs_string_SPAN, strm') = funcs_string_NT(strm)
            val FULL_SPAN = (#1(funcs_string_SPAN), #2(funcs_string_SPAN))
            in
              ((funcs_string_RES), FULL_SPAN, strm')
            end
      fun atom_string_PROD_4 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_string_RES, exp_string_SPAN, strm') = exp_string_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              ((exp_string_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => atom_string_PROD_4(strm)
          | (Tok.STR(_), _, strm') => atom_string_PROD_2(strm)
          | (Tok.ID(_), _, strm') => atom_string_PROD_1(strm)
          | (Tok.KW_TOSTRING, _, strm') => atom_string_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
and op_str_NT (strm) = let
      val (atom_string1_RES, atom_string1_SPAN, strm') = atom_string_NT(strm)
      val (CONCAT_RES, CONCAT_SPAN, strm') = matchCONCAT(strm')
      val (atom_string2_RES, atom_string2_SPAN, strm') = atom_string_NT(strm')
      val FULL_SPAN = (#1(atom_string1_SPAN), #2(atom_string2_SPAN))
      in
        (UserCode.op_str_PROD_1_ACT (atom_string1_RES, atom_string2_RES, CONCAT_RES, atom_string1_SPAN : (Lex.pos * Lex.pos), atom_string2_SPAN : (Lex.pos * Lex.pos), CONCAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
fun assign_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      val (DOTDOT_RES, DOTDOT_SPAN, strm') = matchDOTDOT(strm')
      val (expr_RES, expr_SPAN, strm') = expr_NT(strm')
      val FULL_SPAN = (#1(ID_SPAN), #2(expr_SPAN))
      in
        (UserCode.assign_PROD_1_ACT (ID_RES, expr_RES, DOTDOT_RES, ID_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), DOTDOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
fun prints_NT (strm) = let
      val (KW_Print_RES, KW_Print_SPAN, strm') = matchKW_Print(strm)
      val (LP_RES, LP_SPAN, strm') = matchLP(strm')
      val (exp_string_RES, exp_string_SPAN, strm') = exp_string_NT(strm')
      val (RP_RES, RP_SPAN, strm') = matchRP(strm')
      val FULL_SPAN = (#1(KW_Print_SPAN), #2(RP_SPAN))
      in
        (UserCode.prints_PROD_1_ACT (LP_RES, RP_RES, KW_Print_RES, exp_string_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), exp_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
fun commands_NT (strm) = let
      fun commands_PROD_1 (strm) = let
            val (prints_RES, prints_SPAN, strm') = prints_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(prints_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.commands_PROD_1_ACT (SEMI_RES, prints_RES, SEMI_SPAN : (Lex.pos * Lex.pos), prints_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun commands_PROD_2 (strm) = let
            val (assign_RES, assign_SPAN, strm') = assign_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(assign_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.commands_PROD_2_ACT (SEMI_RES, assign_RES, SEMI_SPAN : (Lex.pos * Lex.pos), assign_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun commands_PROD_3 (strm) = let
            val (conditional_RES, conditional_SPAN, strm') = conditional_NT(strm)
            val FULL_SPAN = (#1(conditional_SPAN), #2(conditional_SPAN))
            in
              (UserCode.commands_PROD_3_ACT (conditional_RES, conditional_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_IF, _, strm') => commands_PROD_3(strm)
          | (Tok.KW_Print, _, strm') => commands_PROD_1(strm)
          | (Tok.ID(_), _, strm') => commands_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and conditional_NT (strm) = let
      val (KW_IF_RES, KW_IF_SPAN, strm') = matchKW_IF(strm)
      val (exp_bool_RES, exp_bool_SPAN, strm') = exp_bool_NT(strm')
      val (KW_THEN_RES, KW_THEN_SPAN, strm') = matchKW_THEN(strm')
      fun conditional_PROD_1_SUBRULE_1_NT (strm) = let
            val (commands_RES, commands_SPAN, strm') = commands_NT(strm)
            val FULL_SPAN = (#1(commands_SPAN), #2(commands_SPAN))
            in
              ((commands_RES), FULL_SPAN, strm')
            end
      fun conditional_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), _, strm') => true
              | (Tok.KW_Print, _, strm') => true
              | (Tok.KW_IF, _, strm') => true
              | _ => false
            (* end case *))
      val (SR1_RES, SR1_SPAN, strm') = EBNF.closure(conditional_PROD_1_SUBRULE_1_PRED, conditional_PROD_1_SUBRULE_1_NT, strm')
      val (KW_ELSE_RES, KW_ELSE_SPAN, strm') = matchKW_ELSE(strm')
      fun conditional_PROD_1_SUBRULE_2_NT (strm) = let
            val (commands_RES, commands_SPAN, strm') = commands_NT(strm)
            val FULL_SPAN = (#1(commands_SPAN), #2(commands_SPAN))
            in
              ((commands_RES), FULL_SPAN, strm')
            end
      fun conditional_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), _, strm') => true
              | (Tok.KW_Print, _, strm') => true
              | (Tok.KW_IF, _, strm') => true
              | _ => false
            (* end case *))
      val (SR2_RES, SR2_SPAN, strm') = EBNF.closure(conditional_PROD_1_SUBRULE_2_PRED, conditional_PROD_1_SUBRULE_2_NT, strm')
      val (KW_END_RES, KW_END_SPAN, strm') = matchKW_END(strm')
      val FULL_SPAN = (#1(KW_IF_SPAN), #2(KW_END_SPAN))
      in
        (UserCode.conditional_PROD_1_ACT (SR1_RES, SR2_RES, exp_bool_RES, KW_ELSE_RES, KW_THEN_RES, KW_IF_RES, KW_END_RES, SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), exp_bool_SPAN : (Lex.pos * Lex.pos), KW_ELSE_SPAN : (Lex.pos * Lex.pos), KW_THEN_SPAN : (Lex.pos * Lex.pos), KW_IF_SPAN : (Lex.pos * Lex.pos), KW_END_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
fun declaration_NT (strm) = let
      val (TIPO_RES, TIPO_SPAN, strm') = matchTIPO(strm)
      val (ID_RES, ID_SPAN, strm') = matchID(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(TIPO_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.declaration_PROD_1_ACT (ID_RES, SEMI_RES, TIPO_RES, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TIPO_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
fun variables_NT (strm) = let
      fun variables_PROD_1_SUBRULE_1_NT (strm) = let
            val (declaration_RES, declaration_SPAN, strm') = declaration_NT(strm)
            val FULL_SPAN = (#1(declaration_SPAN), #2(declaration_SPAN))
            in
              ((declaration_RES), FULL_SPAN, strm')
            end
      fun variables_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.TIPO(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(variables_PROD_1_SUBRULE_1_PRED, variables_PROD_1_SUBRULE_1_NT, strm)
      val (KW_endvars_RES, KW_endvars_SPAN, strm') = matchKW_endvars(strm')
      val FULL_SPAN = (#1(SR_SPAN), #2(KW_endvars_SPAN))
      in
        (UserCode.variables_PROD_1_ACT (SR_RES, KW_endvars_RES, SR_SPAN : (Lex.pos * Lex.pos), KW_endvars_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
fun program_NT (strm) = let
      val (KW_title_RES, KW_title_SPAN, strm') = matchKW_title(strm)
      val (STR_RES, STR_SPAN, strm') = matchSTR(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val (KW_variables_RES, KW_variables_SPAN, strm') = matchKW_variables(strm')
      val (variables_RES, variables_SPAN, strm') = variables_NT(strm')
      val (KW_commands_RES, KW_commands_SPAN, strm') = matchKW_commands(strm')
      fun program_PROD_1_SUBRULE_1_NT (strm) = let
            val (commands_RES, commands_SPAN, strm') = commands_NT(strm)
            val FULL_SPAN = (#1(commands_SPAN), #2(commands_SPAN))
            in
              ((commands_RES), FULL_SPAN, strm')
            end
      fun program_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ID(_), _, strm') => true
              | (Tok.KW_Print, _, strm') => true
              | (Tok.KW_IF, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(program_PROD_1_SUBRULE_1_PRED, program_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(KW_title_SPAN), #2(SR_SPAN))
      in
        (UserCode.program_PROD_1_ACT (SR_RES, STR_RES, SEMI_RES, KW_title_RES, KW_variables_RES, KW_commands_RES, variables_RES, SR_SPAN : (Lex.pos * Lex.pos), STR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_title_SPAN : (Lex.pos * Lex.pos), KW_variables_SPAN : (Lex.pos * Lex.pos), KW_commands_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ts_REFC, tree_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
in
  (program_NT)
end
val program_NT =  fn s => unwrap (Err.launch (eh, lexFn, program_NT , true) s)

in (program_NT) end
  in
fun parse lexFn  s = let val (program_NT) = mk lexFn in program_NT s end

  end

end
