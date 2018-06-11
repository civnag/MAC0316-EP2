structure 
DarwinTokens = struct

    datatype token = EOF
      | KW_MEAN
      | KW_TOSTRING
      | KW_END
      | KW_DO
      | KW_WHILE
      | KW_ELSE
      | KW_THEN
      | KW_IF
      | EMPTY
      | KW_PROD
      | KW_SUM
      | KW_terminate
      | KW_endvars
      | KW_Print
      | STR of string
      | KW_comands
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

    val allToks = [EOF, KW_MEAN, KW_TOSTRING, KW_END, KW_DO, KW_WHILE, KW_ELSE, KW_THEN, KW_IF, EMPTY, KW_PROD, KW_SUM, KW_terminate, KW_endvars, KW_Print, KW_comands, SEMI, KW_variables, NEQ, GEQ, LEQ, LT, GT, SPACE, NOT, OR, AND, RP, LP, COMMA, MINUS, DIV, TIMES, EEQ, DOT, PLUS, EQ, KW_title, KW_in, KW_let]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (KW_MEAN) => "mean"
  | (KW_TOSTRING) => "toString"
  | (KW_END) => "end"
  | (KW_DO) => "do"
  | (KW_WHILE) => "while"
  | (KW_ELSE) => "else"
  | (KW_THEN) => "then"
  | (KW_IF) => "if"
  | (EMPTY) => "{}"
  | (KW_PROD) => "prod"
  | (KW_SUM) => "sum"
  | (KW_terminate) => "terminate"
  | (KW_endvars) => "end variables"
  | (KW_Print) => "print"
  | (STR(_)) => "STR"
  | (KW_comands) => "commands"
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
  | (KW_MEAN) => true
  | (KW_TOSTRING) => true
  | (KW_END) => true
  | (KW_DO) => true
  | (KW_WHILE) => true
  | (KW_ELSE) => true
  | (KW_THEN) => true
  | (KW_IF) => true
  | (EMPTY) => false
  | (KW_PROD) => true
  | (KW_SUM) => true
  | (KW_terminate) => true
  | (KW_endvars) => true
  | (KW_Print) => false
  | (STR(_)) => false
  | (KW_comands) => true
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

 
    fun insere(hm,n,"int") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Int_ 0))
      | insere(hm,n,"string") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.String_ ""))
      | insere(hm,n,"float") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Float_ 0.0))
      | insere(hm,n,_) = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Boolean_ false))
    
    fun getInt x = (Grammar.extractInt x)
    fun getFloat x = (Grammar.extractFloat x)
    fun getBool x = (Grammar.extractBool x)
    fun getList x = (Grammar.extractList x)
    
    fun exprTypes e1 e2 = (Grammar.typeof e1) = (Grammar.typeof e2)
    fun isType e1 t = (Grammar.typeof e1) = t
    
    fun getVar v = AtomMap.appi (fn (k,w) => print (
        let val _ = print(Atom.toString k) 
            val _ = "oi"
            val _ = print (Grammar.show w)
        in 
            ""
        end)) v


fun program_PROD_1_ACT (d, STR, commands, SEMI, KW_title, KW_variables, variables, KW_comands, STR_SPAN : (Lex.pos * Lex.pos), commands_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_title_SPAN : (Lex.pos * Lex.pos), KW_variables_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), KW_comands_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( )
fun commands_PROD_1_ACT (commands, prints, commands_SPAN : (Lex.pos * Lex.pos), prints_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( )
fun commands_PROD_2_ACT (commands, assign, commands_SPAN : (Lex.pos * Lex.pos), assign_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( )
fun commands_PROD_3_ACT (conditional, conditional_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( )
fun assign_PROD_1_ACT (EQ, ID, SEMI, expr, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( v := Grammar.updateHt(!v,Atom.atom ID,expr))
fun expr_PROD_1_ACT (exp_arit, exp_arit_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( exp_arit)
fun expr_PROD_2_ACT (exp_bool, exp_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( exp_bool)
fun expr_PROD_3_ACT (funcs_float, funcs_float_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.Primitivo (Grammar.Float_ funcs_float))
fun expr_PROD_4_ACT (funcs_string, funcs_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.Primitivo (Grammar.String_ funcs_string))
fun expr_PROD_5_ACT (SINT, SINT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  (  Grammar.Sample (List.map (fn(x) => Grammar.Primitivo(Grammar.Int_ x)) SINT) )
fun expr_PROD_6_ACT (SFLOAT, SFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  (  Grammar.Sample (List.map (fn(x) => Grammar.Primitivo(Grammar.Float_ x)) SFLOAT) )
fun expr_PROD_7_ACT (SBOOL, SBOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  (  Grammar.Sample (List.map (fn(x) => Grammar.Primitivo(Grammar.Boolean_ x)) SBOOL) )
fun expr_PROD_8_ACT (SSTRING, SSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  (  Grammar.Sample (List.map (fn(x) => Grammar.Primitivo(Grammar.String_ x)) SSTRING) )
fun prints_PROD_1_ACT (LP, RP, STR, KW_Print, SEMI, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), STR_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  (  ps := (STR::(!ps)))
fun prints_PROD_2_ACT (ID, LP, RP, KW_Print, SEMI, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( 
        let 
            val k = Grammar.show (valOf (AtomMap.find (!v, Atom.atom ID)))
        in 
            ps := k::(!ps)
        end
      )
fun funcs_string_PROD_1_ACT (LP, RP, expr, KW_TOSTRING, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), KW_TOSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.show expr)
fun funcs_float_PROD_1_ACT (LP, RP, EMPTY, KW_SUM, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), EMPTY_SPAN : (Lex.pos * Lex.pos), KW_SUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  (  0.0)
fun funcs_float_PROD_2_ACT (LP, RP, float_list, KW_SUM, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), KW_SUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( List.foldl op+ 0.0 float_list)
fun funcs_float_PROD_3_ACT (LP, RP, KW_PROD, EMPTY, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_PROD_SPAN : (Lex.pos * Lex.pos), EMPTY_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( 0.0)
fun funcs_float_PROD_4_ACT (LP, RP, KW_PROD, float_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_PROD_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( List.foldl op* 1.0 float_list)
fun funcs_float_PROD_5_ACT (LP, RP, KW_MEAN, float_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_MEAN_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Statistics.mean float_list)
fun float_list_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( List.map getFloat (getList (valOf(AtomMap.find (!v, Atom.atom ID)))))
fun float_list_PROD_2_ACT (SFLOAT, SFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( SFLOAT)
fun exp_bool_PROD_1_ACT (EEQ, exp_arit1, exp_arit2, EEQ_SPAN : (Lex.pos * Lex.pos), exp_arit1_SPAN : (Lex.pos * Lex.pos), exp_arit2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.oper("==",exp_arit1,exp_arit2))
fun exp_bool_PROD_2_ACT (NEQ, exp_arit1, exp_arit2, NEQ_SPAN : (Lex.pos * Lex.pos), exp_arit1_SPAN : (Lex.pos * Lex.pos), exp_arit2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.oper("!=",exp_arit1,exp_arit2))
fun exp_bool_PROD_3_ACT (LEQ, exp_arit1, exp_arit2, LEQ_SPAN : (Lex.pos * Lex.pos), exp_arit1_SPAN : (Lex.pos * Lex.pos), exp_arit2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.oper("<=",exp_arit1,exp_arit2))
fun exp_bool_PROD_4_ACT (GEQ, exp_arit1, exp_arit2, GEQ_SPAN : (Lex.pos * Lex.pos), exp_arit1_SPAN : (Lex.pos * Lex.pos), exp_arit2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.oper(">=",exp_arit1,exp_arit2))
fun exp_bool_PROD_5_ACT (LT, exp_arit1, exp_arit2, LT_SPAN : (Lex.pos * Lex.pos), exp_arit1_SPAN : (Lex.pos * Lex.pos), exp_arit2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.oper("<",exp_arit1,exp_arit2))
fun exp_bool_PROD_6_ACT (GT, exp_arit1, exp_arit2, GT_SPAN : (Lex.pos * Lex.pos), exp_arit1_SPAN : (Lex.pos * Lex.pos), exp_arit2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.oper(">",exp_arit1,exp_arit2))
fun exp_bool_PROD_7_ACT (AND, atom_bool1, atom_bool2, AND_SPAN : (Lex.pos * Lex.pos), atom_bool1_SPAN : (Lex.pos * Lex.pos), atom_bool2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( (Grammar.Primitivo(Grammar.Boolean_((getBool atom_bool1) andalso (getBool atom_bool2)))))
fun exp_bool_PROD_8_ACT (OR, atom_bool1, atom_bool2, OR_SPAN : (Lex.pos * Lex.pos), atom_bool1_SPAN : (Lex.pos * Lex.pos), atom_bool2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( (Grammar.Primitivo(Grammar.Boolean_((getBool atom_bool1) orelse (getBool atom_bool2)))))
fun exp_bool_PROD_9_ACT (NOT, atom_bool, NOT_SPAN : (Lex.pos * Lex.pos), atom_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( (Grammar.Primitivo(Grammar.Boolean_(not ((getBool atom_bool))))) )
fun atom_bool_PROD_1_ACT (BOOL, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.Primitivo (Grammar.Boolean_ BOOL))
fun atom_bool_PROD_3_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( valOf(AtomMap.find (!v, Atom.atom ID)))
fun conditional_PROD_1_ACT (exp_bool, KW_IF, block, exp_bool_SPAN : (Lex.pos * Lex.pos), KW_IF_SPAN : (Lex.pos * Lex.pos), block_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( )
fun block_PROD_1_ACT (b, commands, KW_THEN, commands_SPAN : (Lex.pos * Lex.pos), KW_THEN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( if b then commands else ())
fun block_PROD_2_ACT (b, commands, KW_THEN, else_block, commands_SPAN : (Lex.pos * Lex.pos), KW_THEN_SPAN : (Lex.pos * Lex.pos), else_block_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( if b then commands else else_block)
fun addExp_PROD_1_ACT (PLUS, multExp1, multExp2, PLUS_SPAN : (Lex.pos * Lex.pos), multExp1_SPAN : (Lex.pos * Lex.pos), multExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.oper("+",multExp1,multExp2))
fun addExp_PROD_2_ACT (multExp1, multExp2, MINUS, multExp1_SPAN : (Lex.pos * Lex.pos), multExp2_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.oper("-",multExp1,multExp2))
fun multExp_PROD_1_ACT (DIV, prefixExp1, prefixExp2, DIV_SPAN : (Lex.pos * Lex.pos), prefixExp1_SPAN : (Lex.pos * Lex.pos), prefixExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  (  Grammar.oper("/",prefixExp1,prefixExp2) )
fun multExp_PROD_2_ACT (TIMES, prefixExp1, prefixExp2, TIMES_SPAN : (Lex.pos * Lex.pos), prefixExp1_SPAN : (Lex.pos * Lex.pos), prefixExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  (  Grammar.oper("*",prefixExp1,prefixExp2) )
fun prefixExp_PROD_2_ACT (MINUS, prefixExp, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  (  Grammar.oper("neg",prefixExp,prefixExp) )
fun atomicExp_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( valOf(AtomMap.find (!v, Atom.atom ID)))
fun atomicExp_PROD_2_ACT (NUM, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.Primitivo (Grammar.Int_ NUM))
fun atomicExp_PROD_3_ACT (REAL, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( Grammar.Primitivo (Grammar.Float_ REAL))
fun variables_PROD_1_ACT (SR, KW_endvars, SR_SPAN : (Lex.pos * Lex.pos), KW_endvars_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( (fn(_) => ()) SR)
fun declaration_PROD_1_ACT (ID, SEMI, TIPO, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TIPO_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v, ts) = 
  ( v:=insere(!v,Atom.atom ID,Atom.toString(Atom.atom TIPO));
                       ts:=AtomMap.insert(!ts,Atom.atom ID,TIPO))
fun assign_PROD_1_PRED (EQ, ID, SEMI, expr, ps, v, ts) = 
  ( AtomMap.inDomain(!v,Atom.atom ID)
                        andalso (isType expr (valOf (AtomMap.find (!ts, Atom.atom ID)))))
fun exp_bool_PROD_1_PRED (EEQ, exp_arit1, exp_arit2, ps, v, ts) = 
  ( exprTypes exp_arit1 exp_arit2)
fun exp_bool_PROD_2_PRED (NEQ, exp_arit1, exp_arit2, ps, v, ts) = 
  ( exprTypes exp_arit1 exp_arit2)
fun exp_bool_PROD_3_PRED (LEQ, exp_arit1, exp_arit2, ps, v, ts) = 
  ( exprTypes exp_arit1 exp_arit2)
fun exp_bool_PROD_4_PRED (GEQ, exp_arit1, exp_arit2, ps, v, ts) = 
  ( exprTypes exp_arit1 exp_arit2)
fun exp_bool_PROD_5_PRED (LT, exp_arit1, exp_arit2, ps, v, ts) = 
  ( exprTypes exp_arit1 exp_arit2)
fun exp_bool_PROD_6_PRED (GT, exp_arit1, exp_arit2, ps, v, ts) = 
  ( exprTypes exp_arit1 exp_arit2)
fun exp_bool_PROD_7_PRED (AND, atom_bool1, atom_bool2, ps, v, ts) = 
  ( exprTypes atom_bool1 atom_bool2)
fun exp_bool_PROD_8_PRED (OR, atom_bool1, atom_bool2, ps, v, ts) = 
  ( exprTypes atom_bool1 atom_bool2)
fun exp_bool_PROD_9_PRED (NOT, atom_bool, ps, v, ts) = 
  ( isType atom_bool "bool")
fun addExp_PROD_1_PRED (PLUS, multExp1, multExp2, ps, v, ts) = 
  ( exprTypes multExp1 multExp2)
fun addExp_PROD_2_PRED (multExp1, multExp2, MINUS, ps, v, ts) = 
  ( exprTypes multExp1 multExp2)
fun multExp_PROD_1_PRED (DIV, prefixExp1, prefixExp2, ps, v, ts) = 
  ( (isType prefixExp1 "float") andalso (isType prefixExp2 "float") andalso (exprTypes prefixExp1 prefixExp2))
fun multExp_PROD_2_PRED (TIMES, prefixExp1, prefixExp2, ps, v, ts) = 
  ( exprTypes prefixExp1 prefixExp2)
fun ARGS_51 (exp_bool, KW_IF, ps, v, ts) = 
  (getBool exp_bool)
fun ARGS_54 (b, commands, KW_THEN, ps, v, ts) = 
  (b)
fun mkps_REFC() : (string list) ref = ref ( nil)
fun mkv_REFC() : ((Grammar.tipo) AtomMap.map) ref = ref ( AtomMap.empty)
fun mkts_REFC() : (string AtomMap.map) ref = ref ( AtomMap.empty)

    end

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)
    structure EBNF = AntlrEBNF(struct
			         type strm = Err.wstream
			         val getSpan = Err.getSpan
			       end)

    fun mk lexFn = let
val ps_REFC = UserCode.mkps_REFC()
val v_REFC = UserCode.mkv_REFC()
val ts_REFC = UserCode.mkts_REFC()
fun getS() = {ps = !ps_REFC, v = !v_REFC, ts = !ts_REFC}
fun putS{ps, v, ts} = (ps_REFC := ps; v_REFC := v; ts_REFC := ts)
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
fun matchKW_comands strm = (case (lex(strm))
 of (Tok.KW_comands, span, strm') => ((), span, strm')
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
fun exp_arit_NT (strm) = let
      fun exp_arit_PROD_1 (strm) = let
            val (addExp_RES, addExp_SPAN, strm') = addExp_NT(strm)
            val FULL_SPAN = (#1(addExp_SPAN), #2(addExp_SPAN))
            in
              ((addExp_RES), FULL_SPAN, strm')
            end
      fun exp_arit_PROD_2 (strm) = let
            val (atomicExp_RES, atomicExp_SPAN, strm') = atomicExp_NT(strm)
            val FULL_SPAN = (#1(atomicExp_SPAN), #2(atomicExp_SPAN))
            in
              ((atomicExp_RES), FULL_SPAN, strm')
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
          | (Tok.MINUS, _, strm') => exp_arit_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and atomicExp_NT (strm) = let
      fun atomicExp_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.atomicExp_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun atomicExp_PROD_2 (strm) = let
            val (NUM_RES, NUM_SPAN, strm') = matchNUM(strm)
            val FULL_SPAN = (#1(NUM_SPAN), #2(NUM_SPAN))
            in
              (UserCode.atomicExp_PROD_2_ACT (NUM_RES, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun atomicExp_PROD_3 (strm) = let
            val (REAL_RES, REAL_SPAN, strm') = matchREAL(strm)
            val FULL_SPAN = (#1(REAL_SPAN), #2(REAL_SPAN))
            in
              (UserCode.atomicExp_PROD_3_ACT (REAL_RES, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
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
and addExp_NT (strm) = let
      fun addExp_PROD_1 (strm) = let
            val (multExp1_RES, multExp1_SPAN, strm') = multExp_NT(strm)
            val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm')
            val (multExp2_RES, multExp2_SPAN, strm') = multExp_NT(strm')
            in
              if (UserCode.addExp_PROD_1_PRED (PLUS_RES, multExp1_RES, multExp2_RES, ps_REFC, v_REFC, ts_REFC))
                then let
                  val FULL_SPAN = (#1(multExp1_SPAN), #2(multExp2_SPAN))
                  in
                    (UserCode.addExp_PROD_1_ACT (PLUS_RES, multExp1_RES, multExp2_RES, PLUS_SPAN : (Lex.pos * Lex.pos), multExp1_SPAN : (Lex.pos * Lex.pos), multExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun addExp_PROD_2 (strm) = let
            val (multExp1_RES, multExp1_SPAN, strm') = multExp_NT(strm)
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm')
            val (multExp2_RES, multExp2_SPAN, strm') = multExp_NT(strm')
            in
              if (UserCode.addExp_PROD_2_PRED (multExp1_RES, multExp2_RES, MINUS_RES, ps_REFC, v_REFC, ts_REFC))
                then let
                  val FULL_SPAN = (#1(multExp1_SPAN), #2(multExp2_SPAN))
                  in
                    (UserCode.addExp_PROD_2_ACT (multExp1_RES, multExp2_RES, MINUS_RES, multExp1_SPAN : (Lex.pos * Lex.pos), multExp2_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun addExp_PROD_3 (strm) = let
            val (multExp_RES, multExp_SPAN, strm') = multExp_NT(strm)
            val FULL_SPAN = (#1(multExp_SPAN), #2(multExp_SPAN))
            in
              ((multExp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.NUM(_), _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.REAL(_), _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.MINUS, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [addExp_PROD_1, addExp_PROD_2, addExp_PROD_3])
          | _ => fail()
        (* end case *))
      end
and multExp_NT (strm) = let
      fun multExp_PROD_1 (strm) = let
            val (prefixExp1_RES, prefixExp1_SPAN, strm') = prefixExp_NT(strm)
            val (DIV_RES, DIV_SPAN, strm') = matchDIV(strm')
            val (prefixExp2_RES, prefixExp2_SPAN, strm') = prefixExp_NT(strm')
            in
              if (UserCode.multExp_PROD_1_PRED (DIV_RES, prefixExp1_RES, prefixExp2_RES, ps_REFC, v_REFC, ts_REFC))
                then let
                  val FULL_SPAN = (#1(prefixExp1_SPAN), #2(prefixExp2_SPAN))
                  in
                    (UserCode.multExp_PROD_1_ACT (DIV_RES, prefixExp1_RES, prefixExp2_RES, DIV_SPAN : (Lex.pos * Lex.pos), prefixExp1_SPAN : (Lex.pos * Lex.pos), prefixExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun multExp_PROD_2 (strm) = let
            val (prefixExp1_RES, prefixExp1_SPAN, strm') = prefixExp_NT(strm)
            val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm')
            val (prefixExp2_RES, prefixExp2_SPAN, strm') = prefixExp_NT(strm')
            in
              if (UserCode.multExp_PROD_2_PRED (TIMES_RES, prefixExp1_RES, prefixExp2_RES, ps_REFC, v_REFC, ts_REFC))
                then let
                  val FULL_SPAN = (#1(prefixExp1_SPAN), #2(prefixExp2_SPAN))
                  in
                    (UserCode.multExp_PROD_2_ACT (TIMES_RES, prefixExp1_RES, prefixExp2_RES, TIMES_SPAN : (Lex.pos * Lex.pos), prefixExp1_SPAN : (Lex.pos * Lex.pos), prefixExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun multExp_PROD_3 (strm) = let
            val (atomicExp_RES, atomicExp_SPAN, strm') = atomicExp_NT(strm)
            val FULL_SPAN = (#1(atomicExp_SPAN), #2(atomicExp_SPAN))
            in
              ((atomicExp_RES), FULL_SPAN, strm')
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
          | (Tok.MINUS, _, strm') =>
              tryProds(strm, [multExp_PROD_1, multExp_PROD_2])
          | _ => fail()
        (* end case *))
      end
and prefixExp_NT (strm) = let
      fun prefixExp_PROD_1 (strm) = let
            val (atomicExp_RES, atomicExp_SPAN, strm') = atomicExp_NT(strm)
            val FULL_SPAN = (#1(atomicExp_SPAN), #2(atomicExp_SPAN))
            in
              ((atomicExp_RES), FULL_SPAN, strm')
            end
      fun prefixExp_PROD_2 (strm) = let
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
            val (prefixExp_RES, prefixExp_SPAN, strm') = prefixExp_NT(strm')
            val FULL_SPAN = (#1(MINUS_SPAN), #2(prefixExp_SPAN))
            in
              (UserCode.prefixExp_PROD_2_ACT (MINUS_RES, prefixExp_RES, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
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
fun exp_bool_NT (strm) = let
      fun exp_bool_PROD_1 (strm) = let
            val (exp_arit1_RES, exp_arit1_SPAN, strm') = exp_arit_NT(strm)
            val (EEQ_RES, EEQ_SPAN, strm') = matchEEQ(strm')
            val (exp_arit2_RES, exp_arit2_SPAN, strm') = exp_arit_NT(strm')
            in
              if (UserCode.exp_bool_PROD_1_PRED (EEQ_RES, exp_arit1_RES, exp_arit2_RES, ps_REFC, v_REFC, ts_REFC))
                then let
                  val FULL_SPAN = (#1(exp_arit1_SPAN), #2(exp_arit2_SPAN))
                  in
                    (UserCode.exp_bool_PROD_1_ACT (EEQ_RES, exp_arit1_RES, exp_arit2_RES, EEQ_SPAN : (Lex.pos * Lex.pos), exp_arit1_SPAN : (Lex.pos * Lex.pos), exp_arit2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun exp_bool_PROD_2 (strm) = let
            val (exp_arit1_RES, exp_arit1_SPAN, strm') = exp_arit_NT(strm)
            val (NEQ_RES, NEQ_SPAN, strm') = matchNEQ(strm')
            val (exp_arit2_RES, exp_arit2_SPAN, strm') = exp_arit_NT(strm')
            in
              if (UserCode.exp_bool_PROD_2_PRED (NEQ_RES, exp_arit1_RES, exp_arit2_RES, ps_REFC, v_REFC, ts_REFC))
                then let
                  val FULL_SPAN = (#1(exp_arit1_SPAN), #2(exp_arit2_SPAN))
                  in
                    (UserCode.exp_bool_PROD_2_ACT (NEQ_RES, exp_arit1_RES, exp_arit2_RES, NEQ_SPAN : (Lex.pos * Lex.pos), exp_arit1_SPAN : (Lex.pos * Lex.pos), exp_arit2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun exp_bool_PROD_3 (strm) = let
            val (exp_arit1_RES, exp_arit1_SPAN, strm') = exp_arit_NT(strm)
            val (LEQ_RES, LEQ_SPAN, strm') = matchLEQ(strm')
            val (exp_arit2_RES, exp_arit2_SPAN, strm') = exp_arit_NT(strm')
            in
              if (UserCode.exp_bool_PROD_3_PRED (LEQ_RES, exp_arit1_RES, exp_arit2_RES, ps_REFC, v_REFC, ts_REFC))
                then let
                  val FULL_SPAN = (#1(exp_arit1_SPAN), #2(exp_arit2_SPAN))
                  in
                    (UserCode.exp_bool_PROD_3_ACT (LEQ_RES, exp_arit1_RES, exp_arit2_RES, LEQ_SPAN : (Lex.pos * Lex.pos), exp_arit1_SPAN : (Lex.pos * Lex.pos), exp_arit2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun exp_bool_PROD_4 (strm) = let
            val (exp_arit1_RES, exp_arit1_SPAN, strm') = exp_arit_NT(strm)
            val (GEQ_RES, GEQ_SPAN, strm') = matchGEQ(strm')
            val (exp_arit2_RES, exp_arit2_SPAN, strm') = exp_arit_NT(strm')
            in
              if (UserCode.exp_bool_PROD_4_PRED (GEQ_RES, exp_arit1_RES, exp_arit2_RES, ps_REFC, v_REFC, ts_REFC))
                then let
                  val FULL_SPAN = (#1(exp_arit1_SPAN), #2(exp_arit2_SPAN))
                  in
                    (UserCode.exp_bool_PROD_4_ACT (GEQ_RES, exp_arit1_RES, exp_arit2_RES, GEQ_SPAN : (Lex.pos * Lex.pos), exp_arit1_SPAN : (Lex.pos * Lex.pos), exp_arit2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun exp_bool_PROD_5 (strm) = let
            val (exp_arit1_RES, exp_arit1_SPAN, strm') = exp_arit_NT(strm)
            val (LT_RES, LT_SPAN, strm') = matchLT(strm')
            val (exp_arit2_RES, exp_arit2_SPAN, strm') = exp_arit_NT(strm')
            in
              if (UserCode.exp_bool_PROD_5_PRED (LT_RES, exp_arit1_RES, exp_arit2_RES, ps_REFC, v_REFC, ts_REFC))
                then let
                  val FULL_SPAN = (#1(exp_arit1_SPAN), #2(exp_arit2_SPAN))
                  in
                    (UserCode.exp_bool_PROD_5_ACT (LT_RES, exp_arit1_RES, exp_arit2_RES, LT_SPAN : (Lex.pos * Lex.pos), exp_arit1_SPAN : (Lex.pos * Lex.pos), exp_arit2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun exp_bool_PROD_6 (strm) = let
            val (exp_arit1_RES, exp_arit1_SPAN, strm') = exp_arit_NT(strm)
            val (GT_RES, GT_SPAN, strm') = matchGT(strm')
            val (exp_arit2_RES, exp_arit2_SPAN, strm') = exp_arit_NT(strm')
            in
              if (UserCode.exp_bool_PROD_6_PRED (GT_RES, exp_arit1_RES, exp_arit2_RES, ps_REFC, v_REFC, ts_REFC))
                then let
                  val FULL_SPAN = (#1(exp_arit1_SPAN), #2(exp_arit2_SPAN))
                  in
                    (UserCode.exp_bool_PROD_6_ACT (GT_RES, exp_arit1_RES, exp_arit2_RES, GT_SPAN : (Lex.pos * Lex.pos), exp_arit1_SPAN : (Lex.pos * Lex.pos), exp_arit2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun exp_bool_PROD_7 (strm) = let
            val (atom_bool1_RES, atom_bool1_SPAN, strm') = atom_bool_NT(strm)
            val (AND_RES, AND_SPAN, strm') = matchAND(strm')
            val (atom_bool2_RES, atom_bool2_SPAN, strm') = atom_bool_NT(strm')
            in
              if (UserCode.exp_bool_PROD_7_PRED (AND_RES, atom_bool1_RES, atom_bool2_RES, ps_REFC, v_REFC, ts_REFC))
                then let
                  val FULL_SPAN = (#1(atom_bool1_SPAN), #2(atom_bool2_SPAN))
                  in
                    (UserCode.exp_bool_PROD_7_ACT (AND_RES, atom_bool1_RES, atom_bool2_RES, AND_SPAN : (Lex.pos * Lex.pos), atom_bool1_SPAN : (Lex.pos * Lex.pos), atom_bool2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun exp_bool_PROD_8 (strm) = let
            val (atom_bool1_RES, atom_bool1_SPAN, strm') = atom_bool_NT(strm)
            val (OR_RES, OR_SPAN, strm') = matchOR(strm')
            val (atom_bool2_RES, atom_bool2_SPAN, strm') = atom_bool_NT(strm')
            in
              if (UserCode.exp_bool_PROD_8_PRED (OR_RES, atom_bool1_RES, atom_bool2_RES, ps_REFC, v_REFC, ts_REFC))
                then let
                  val FULL_SPAN = (#1(atom_bool1_SPAN), #2(atom_bool2_SPAN))
                  in
                    (UserCode.exp_bool_PROD_8_ACT (OR_RES, atom_bool1_RES, atom_bool2_RES, OR_SPAN : (Lex.pos * Lex.pos), atom_bool1_SPAN : (Lex.pos * Lex.pos), atom_bool2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun exp_bool_PROD_9 (strm) = let
            val (NOT_RES, NOT_SPAN, strm') = matchNOT(strm)
            val (atom_bool_RES, atom_bool_SPAN, strm') = atom_bool_NT(strm')
            in
              if (UserCode.exp_bool_PROD_9_PRED (NOT_RES, atom_bool_RES, ps_REFC, v_REFC, ts_REFC))
                then let
                  val FULL_SPAN = (#1(NOT_SPAN), #2(atom_bool_SPAN))
                  in
                    (UserCode.exp_bool_PROD_9_ACT (NOT_RES, atom_bool_RES, NOT_SPAN : (Lex.pos * Lex.pos), atom_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun exp_bool_PROD_10 (strm) = let
            val (atom_bool_RES, atom_bool_SPAN, strm') = atom_bool_NT(strm)
            val FULL_SPAN = (#1(atom_bool_SPAN), #2(atom_bool_SPAN))
            in
              ((atom_bool_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.NUM(_), _, strm') =>
              tryProds(strm, [exp_bool_PROD_1, exp_bool_PROD_2,
                exp_bool_PROD_3, exp_bool_PROD_4, exp_bool_PROD_5,
                exp_bool_PROD_6])
          | (Tok.REAL(_), _, strm') =>
              tryProds(strm, [exp_bool_PROD_1, exp_bool_PROD_2,
                exp_bool_PROD_3, exp_bool_PROD_4, exp_bool_PROD_5,
                exp_bool_PROD_6])
          | (Tok.MINUS, _, strm') =>
              tryProds(strm, [exp_bool_PROD_1, exp_bool_PROD_2,
                exp_bool_PROD_3, exp_bool_PROD_4, exp_bool_PROD_5,
                exp_bool_PROD_6])
          | (Tok.ID(_), _, strm') =>
              tryProds(strm, [exp_bool_PROD_1, exp_bool_PROD_2,
                exp_bool_PROD_3, exp_bool_PROD_4, exp_bool_PROD_5,
                exp_bool_PROD_6, exp_bool_PROD_7, exp_bool_PROD_8,
                exp_bool_PROD_10])
          | (Tok.LP, _, strm') =>
              tryProds(strm, [exp_bool_PROD_1, exp_bool_PROD_2,
                exp_bool_PROD_3, exp_bool_PROD_4, exp_bool_PROD_5,
                exp_bool_PROD_6, exp_bool_PROD_7, exp_bool_PROD_8,
                exp_bool_PROD_10])
          | (Tok.BOOL(_), _, strm') =>
              tryProds(strm, [exp_bool_PROD_7, exp_bool_PROD_8,
                exp_bool_PROD_10])
          | (Tok.NOT, _, strm') => exp_bool_PROD_9(strm)
          | _ => fail()
        (* end case *))
      end
and atom_bool_NT (strm) = let
      fun atom_bool_PROD_1 (strm) = let
            val (BOOL_RES, BOOL_SPAN, strm') = matchBOOL(strm)
            val FULL_SPAN = (#1(BOOL_SPAN), #2(BOOL_SPAN))
            in
              (UserCode.atom_bool_PROD_1_ACT (BOOL_RES, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun atom_bool_PROD_2 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_bool_RES, exp_bool_SPAN, strm') = exp_bool_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              ((exp_bool_RES), FULL_SPAN, strm')
            end
      fun atom_bool_PROD_3 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.atom_bool_PROD_3_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => atom_bool_PROD_3(strm)
          | (Tok.BOOL(_), _, strm') => atom_bool_PROD_1(strm)
          | (Tok.LP, _, strm') => atom_bool_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun float_list_NT (strm) = let
      fun float_list_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.float_list_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun float_list_PROD_2 (strm) = let
            val (SFLOAT_RES, SFLOAT_SPAN, strm') = matchSFLOAT(strm)
            val FULL_SPAN = (#1(SFLOAT_SPAN), #2(SFLOAT_SPAN))
            in
              (UserCode.float_list_PROD_2_ACT (SFLOAT_RES, SFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
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
              (UserCode.funcs_float_PROD_1_ACT (LP_RES, RP_RES, EMPTY_RES, KW_SUM_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), EMPTY_SPAN : (Lex.pos * Lex.pos), KW_SUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_2 (strm) = let
            val (KW_SUM_RES, KW_SUM_SPAN, strm') = matchKW_SUM(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list_RES, float_list_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_SUM_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_2_ACT (LP_RES, RP_RES, float_list_RES, KW_SUM_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), KW_SUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_3 (strm) = let
            val (KW_PROD_RES, KW_PROD_SPAN, strm') = matchKW_PROD(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (EMPTY_RES, EMPTY_SPAN, strm') = matchEMPTY(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_PROD_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_3_ACT (LP_RES, RP_RES, KW_PROD_RES, EMPTY_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_PROD_SPAN : (Lex.pos * Lex.pos), EMPTY_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_4 (strm) = let
            val (KW_PROD_RES, KW_PROD_SPAN, strm') = matchKW_PROD(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list_RES, float_list_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_PROD_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_4_ACT (LP_RES, RP_RES, KW_PROD_RES, float_list_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_PROD_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun funcs_float_PROD_5 (strm) = let
            val (KW_MEAN_RES, KW_MEAN_SPAN, strm') = matchKW_MEAN(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (float_list_RES, float_list_SPAN, strm') = float_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(KW_MEAN_SPAN), #2(RP_SPAN))
            in
              (UserCode.funcs_float_PROD_5_ACT (LP_RES, RP_RES, KW_MEAN_RES, float_list_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_MEAN_SPAN : (Lex.pos * Lex.pos), float_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_MEAN, _, strm') => funcs_float_PROD_5(strm)
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
          | _ => fail()
        (* end case *))
      end
fun expr_NT (strm) = let
      fun expr_PROD_1 (strm) = let
            val (exp_arit_RES, exp_arit_SPAN, strm') = exp_arit_NT(strm)
            val FULL_SPAN = (#1(exp_arit_SPAN), #2(exp_arit_SPAN))
            in
              (UserCode.expr_PROD_1_ACT (exp_arit_RES, exp_arit_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_2 (strm) = let
            val (exp_bool_RES, exp_bool_SPAN, strm') = exp_bool_NT(strm)
            val FULL_SPAN = (#1(exp_bool_SPAN), #2(exp_bool_SPAN))
            in
              (UserCode.expr_PROD_2_ACT (exp_bool_RES, exp_bool_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_3 (strm) = let
            val (funcs_float_RES, funcs_float_SPAN, strm') = funcs_float_NT(strm)
            val FULL_SPAN = (#1(funcs_float_SPAN), #2(funcs_float_SPAN))
            in
              (UserCode.expr_PROD_3_ACT (funcs_float_RES, funcs_float_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_4 (strm) = let
            val (funcs_string_RES, funcs_string_SPAN, strm') = funcs_string_NT(strm)
            val FULL_SPAN = (#1(funcs_string_SPAN), #2(funcs_string_SPAN))
            in
              (UserCode.expr_PROD_4_ACT (funcs_string_RES, funcs_string_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_5 (strm) = let
            val (SINT_RES, SINT_SPAN, strm') = matchSINT(strm)
            val FULL_SPAN = (#1(SINT_SPAN), #2(SINT_SPAN))
            in
              (UserCode.expr_PROD_5_ACT (SINT_RES, SINT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_6 (strm) = let
            val (SFLOAT_RES, SFLOAT_SPAN, strm') = matchSFLOAT(strm)
            val FULL_SPAN = (#1(SFLOAT_SPAN), #2(SFLOAT_SPAN))
            in
              (UserCode.expr_PROD_6_ACT (SFLOAT_RES, SFLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_7 (strm) = let
            val (SBOOL_RES, SBOOL_SPAN, strm') = matchSBOOL(strm)
            val FULL_SPAN = (#1(SBOOL_SPAN), #2(SBOOL_SPAN))
            in
              (UserCode.expr_PROD_7_ACT (SBOOL_RES, SBOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun expr_PROD_8 (strm) = let
            val (SSTRING_RES, SSTRING_SPAN, strm') = matchSSTRING(strm)
            val FULL_SPAN = (#1(SSTRING_SPAN), #2(SSTRING_SPAN))
            in
              (UserCode.expr_PROD_8_ACT (SSTRING_RES, SSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.SSTRING(_), _, strm') => expr_PROD_8(strm)
          | (Tok.SFLOAT(_), _, strm') => expr_PROD_6(strm)
          | (Tok.KW_TOSTRING, _, strm') => expr_PROD_4(strm)
          | (Tok.BOOL(_), _, strm') => expr_PROD_2(strm)
          | (Tok.NOT, _, strm') => expr_PROD_2(strm)
          | (Tok.ID(_), _, strm') => tryProds(strm, [expr_PROD_1, expr_PROD_2])
          | (Tok.NUM(_), _, strm') =>
              tryProds(strm, [expr_PROD_1, expr_PROD_2])
          | (Tok.REAL(_), _, strm') =>
              tryProds(strm, [expr_PROD_1, expr_PROD_2])
          | (Tok.MINUS, _, strm') => tryProds(strm, [expr_PROD_1, expr_PROD_2])
          | (Tok.LP, _, strm') => tryProds(strm, [expr_PROD_1, expr_PROD_2])
          | (Tok.KW_SUM, _, strm') => expr_PROD_3(strm)
          | (Tok.KW_PROD, _, strm') => expr_PROD_3(strm)
          | (Tok.KW_MEAN, _, strm') => expr_PROD_3(strm)
          | (Tok.SINT(_), _, strm') => expr_PROD_5(strm)
          | (Tok.SBOOL(_), _, strm') => expr_PROD_7(strm)
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
        (UserCode.funcs_string_PROD_1_ACT (LP_RES, RP_RES, expr_RES, KW_TOSTRING_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), KW_TOSTRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
          FULL_SPAN, strm')
      end
fun assign_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
      val (expr_RES, expr_SPAN, strm') = expr_NT(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      in
        if (UserCode.assign_PROD_1_PRED (EQ_RES, ID_RES, SEMI_RES, expr_RES, ps_REFC, v_REFC, ts_REFC))
          then let
            val FULL_SPAN = (#1(ID_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.assign_PROD_1_ACT (EQ_RES, ID_RES, SEMI_RES, expr_RES, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
          else fail()
      end
fun prints_NT (strm) = let
      fun prints_PROD_1 (strm) = let
            val (KW_Print_RES, KW_Print_SPAN, strm') = matchKW_Print(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (STR_RES, STR_SPAN, strm') = matchSTR(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_Print_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.prints_PROD_1_ACT (LP_RES, RP_RES, STR_RES, KW_Print_RES, SEMI_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), STR_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun prints_PROD_2 (strm) = let
            val (KW_Print_RES, KW_Print_SPAN, strm') = matchKW_Print(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_Print_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.prints_PROD_2_ACT (ID_RES, LP_RES, RP_RES, KW_Print_RES, SEMI_RES, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_Print, _, strm') =>
              (case (lex(strm'))
               of (Tok.LP, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.ID(_), _, strm') => prints_PROD_2(strm)
                      | (Tok.STR(_), _, strm') => prints_PROD_1(strm)
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun commands_NT (strm) = let
      fun commands_PROD_1 (strm) = let
            val (prints_RES, prints_SPAN, strm') = prints_NT(strm)
            fun commands_PROD_1_SUBRULE_1_NT (strm) = let
                  val (commands_RES, commands_SPAN, strm') = commands_NT(strm)
                  val FULL_SPAN = (#1(commands_SPAN), #2(commands_SPAN))
                  in
                    ((commands_RES), FULL_SPAN, strm')
                  end
            fun commands_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.ID(_), _, strm') => true
                    | (Tok.KW_Print, _, strm') => true
                    | (Tok.KW_IF, _, strm') => true
                    | _ => false
                  (* end case *))
            val (commands_RES, commands_SPAN, strm') = EBNF.optional(commands_PROD_1_SUBRULE_1_PRED, commands_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(prints_SPAN), #2(commands_SPAN))
            in
              (UserCode.commands_PROD_1_ACT (commands_RES, prints_RES, commands_SPAN : (Lex.pos * Lex.pos), prints_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun commands_PROD_2 (strm) = let
            val (assign_RES, assign_SPAN, strm') = assign_NT(strm)
            fun commands_PROD_2_SUBRULE_1_NT (strm) = let
                  val (commands_RES, commands_SPAN, strm') = commands_NT(strm)
                  val FULL_SPAN = (#1(commands_SPAN), #2(commands_SPAN))
                  in
                    ((commands_RES), FULL_SPAN, strm')
                  end
            fun commands_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.ID(_), _, strm') => true
                    | (Tok.KW_Print, _, strm') => true
                    | (Tok.KW_IF, _, strm') => true
                    | _ => false
                  (* end case *))
            val (commands_RES, commands_SPAN, strm') = EBNF.optional(commands_PROD_2_SUBRULE_1_PRED, commands_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(assign_SPAN), #2(commands_SPAN))
            in
              (UserCode.commands_PROD_2_ACT (commands_RES, assign_RES, commands_SPAN : (Lex.pos * Lex.pos), assign_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun commands_PROD_3 (strm) = let
            val (conditional_RES, conditional_SPAN, strm') = conditional_NT(strm)
            val FULL_SPAN = (#1(conditional_SPAN), #2(conditional_SPAN))
            in
              (UserCode.commands_PROD_3_ACT (conditional_RES, conditional_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
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
      val (block_RES, block_SPAN, strm') = (block_NT (UserCode.ARGS_51 (exp_bool_RES, KW_IF_RES, ps_REFC, v_REFC, ts_REFC)))(strm')
      val FULL_SPAN = (#1(KW_IF_SPAN), #2(block_SPAN))
      in
        (UserCode.conditional_PROD_1_ACT (exp_bool_RES, KW_IF_RES, block_RES, exp_bool_SPAN : (Lex.pos * Lex.pos), KW_IF_SPAN : (Lex.pos * Lex.pos), block_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
          FULL_SPAN, strm')
      end
and block_NT (b_RES) (strm) = let
      fun block_PROD_1 (strm) = let
            val (KW_THEN_RES, KW_THEN_SPAN, strm') = matchKW_THEN(strm)
            val (commands_RES, commands_SPAN, strm') = commands_NT(strm')
            val FULL_SPAN = (#1(KW_THEN_SPAN), #2(commands_SPAN))
            in
              (UserCode.block_PROD_1_ACT (b_RES, commands_RES, KW_THEN_RES, commands_SPAN : (Lex.pos * Lex.pos), KW_THEN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      fun block_PROD_2 (strm) = let
            val (KW_THEN_RES, KW_THEN_SPAN, strm') = matchKW_THEN(strm)
            val (commands_RES, commands_SPAN, strm') = commands_NT(strm')
            val (else_block_RES, else_block_SPAN, strm') = (else_block_NT (UserCode.ARGS_54 (b_RES, commands_RES, KW_THEN_RES, ps_REFC, v_REFC, ts_REFC)))(strm')
            val FULL_SPAN = (#1(KW_THEN_SPAN), #2(else_block_SPAN))
            in
              (UserCode.block_PROD_2_ACT (b_RES, commands_RES, KW_THEN_RES, else_block_RES, commands_SPAN : (Lex.pos * Lex.pos), KW_THEN_SPAN : (Lex.pos * Lex.pos), else_block_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_THEN, _, strm') =>
              tryProds(strm, [block_PROD_1, block_PROD_2])
          | _ => fail()
        (* end case *))
      end
and else_block_NT (b_RES) (strm) = let
      val (KW_ELSE_RES, KW_ELSE_SPAN, strm') = matchKW_ELSE(strm)
      val (commands_RES, commands_SPAN, strm') = commands_NT(strm')
      val FULL_SPAN = (#1(KW_ELSE_SPAN), #2(commands_SPAN))
      in
        ((commands_RES), FULL_SPAN, strm')
      end
fun declaration_NT (strm) = let
      val (TIPO_RES, TIPO_SPAN, strm') = matchTIPO(strm)
      val (ID_RES, ID_SPAN, strm') = matchID(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(TIPO_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.declaration_PROD_1_ACT (ID_RES, SEMI_RES, TIPO_RES, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TIPO_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
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
        (UserCode.variables_PROD_1_ACT (SR_RES, KW_endvars_RES, SR_SPAN : (Lex.pos * Lex.pos), KW_endvars_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
          FULL_SPAN, strm')
      end
fun program_NT (d_RES) (strm) = let
      val (KW_title_RES, KW_title_SPAN, strm') = matchKW_title(strm)
      val (STR_RES, STR_SPAN, strm') = matchSTR(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val (KW_variables_RES, KW_variables_SPAN, strm') = matchKW_variables(strm')
      val (variables_RES, variables_SPAN, strm') = variables_NT(strm')
      val (KW_comands_RES, KW_comands_SPAN, strm') = matchKW_comands(strm')
      val (commands_RES, commands_SPAN, strm') = commands_NT(strm')
      val FULL_SPAN = (#1(KW_title_SPAN), #2(commands_SPAN))
      in
        (UserCode.program_PROD_1_ACT (d_RES, STR_RES, commands_RES, SEMI_RES, KW_title_RES, KW_variables_RES, variables_RES, KW_comands_RES, STR_SPAN : (Lex.pos * Lex.pos), commands_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_title_SPAN : (Lex.pos * Lex.pos), KW_variables_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), KW_comands_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC, ts_REFC),
          FULL_SPAN, strm')
      end
in
  (program_NT)
end
val program_NT =  fn x => fn s => unwrap (Err.launch (eh, lexFn, program_NT x , true) s)

in (program_NT) end
  in
fun parse lexFn  x s = let val (program_NT) = mk lexFn in program_NT x s end

  end

end
