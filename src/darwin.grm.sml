structure 
DarwinTokens = struct

    datatype token = EOF
      | KW_endvars
      | KW_Print
      | STR of string
      | KW_comands
      | TIPO of string
      | SEMI
      | KW_variables
      | SPACE
      | RP
      | LP
      | MINUS
      | TIMES
      | PLUS
      | EQ
      | NUM of Int.int
      | ID of string
      | KW_title
      | KW_in
      | KW_let

    val allToks = [EOF, KW_endvars, KW_Print, KW_comands, SEMI, KW_variables, SPACE, RP, LP, MINUS, TIMES, PLUS, EQ, KW_title, KW_in, KW_let]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (KW_endvars) => "end variables"
  | (KW_Print) => "print"
  | (STR(_)) => "STR"
  | (KW_comands) => "commands"
  | (TIPO(_)) => "TIPO"
  | (SEMI) => "SEMI"
  | (KW_variables) => "variables"
  | (SPACE) => " "
  | (RP) => ")"
  | (LP) => "("
  | (MINUS) => "-"
  | (TIMES) => "*"
  | (PLUS) => "+"
  | (EQ) => "="
  | (NUM(_)) => "NUM"
  | (ID(_)) => "ID"
  | (KW_title) => "title"
  | (KW_in) => "in"
  | (KW_let) => "let"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (KW_endvars) => true
  | (KW_Print) => false
  | (STR(_)) => false
  | (KW_comands) => true
  | (TIPO(_)) => false
  | (SEMI) => false
  | (KW_variables) => true
  | (SPACE) => false
  | (RP) => false
  | (LP) => false
  | (MINUS) => false
  | (TIMES) => false
  | (PLUS) => false
  | (EQ) => false
  | (NUM(_)) => false
  | (ID(_)) => false
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

 
    fun insere(hm,n,"int") = AtomMap.insert(hm, n, Grammar.Int_ 0)
      | insere(hm,n,"string") = AtomMap.insert(hm, n, Grammar.String_ "")
      | insere(hm,n,"float") = AtomMap.insert(hm, n, Grammar.Float_ 0.0)
      | insere(hm,n,_) = AtomMap.insert(hm, n, Grammar.Boolean_ false)


fun program_PROD_1_ACT (v, ps, STR, env, exp, commands, SEMI, KW_title, KW_variables, variables, KW_comands, STR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), commands_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_title_SPAN : (Lex.pos * Lex.pos), KW_variables_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), KW_comands_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ps)
fun commands_PROD_1_ACT (v, LP, RP, ps, STR, KW_Print, SEMI, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), STR_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( STR::ps)
fun commands_PROD_2_ACT (v, ID, LP, RP, ps, KW_Print, SEMI, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( 
        let 
            val v = AtomMap.find (v, Atom.atom ID)
        in 
            (Grammar.show (valOf v))::ps
        end
      )
fun exp_PROD_1_ACT (EQ, ID, env, exp1, exp2, KW_in, KW_let, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  exp2 )
fun addExp_PROD_1_ACT (SR, env, multExp, SR_SPAN : (Lex.pos * Lex.pos), multExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  List.foldr op+ 0 (multExp::SR) )
fun multExp_PROD_1_ACT (SR, env, prefixExp, SR_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  List.foldr op* 1 (prefixExp::SR) )
fun prefixExp_PROD_2_ACT (env, MINUS, prefixExp, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  ~prefixExp )
fun atomicExp_PROD_1_ACT (ID, env, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  valOf(AtomMap.find (env, Atom.atom ID)) )
fun variables_PROD_2_ACT (v, KW_endvars, KW_endvars_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( print "tchau "; v)
fun declaration_PROD_1_ACT (v, ID, SEMI, TIPO, variables, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TIPO_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( v)
fun ARGS_4 (v, ps, STR, env, SEMI, KW_title, KW_variables) = 
  (v)
fun ARGS_5 (v, ps, STR, env, SEMI, KW_title, KW_variables, variables, KW_comands) = 
  (v,ps)
fun ARGS_6 (v, ps, STR, env, commands, SEMI, KW_title, KW_variables, variables, KW_comands) = 
  (env)
fun ARGS_10 (EQ, ID, env, KW_let) = 
  (env)
fun ARGS_11 (EQ, ID, env, exp1, KW_in, KW_let) = 
  (AtomMap.insert(env, Atom.atom ID, exp1))
fun ARGS_12 (env) = 
  (env)
fun ARGS_15 (env, PLUS, multExp) = 
  (env)
fun ARGS_14 (env) = 
  (env)
fun ARGS_18 (env, TIMES, prefixExp) = 
  (env)
fun ARGS_17 (env) = 
  (env)
fun ARGS_19 (env) = 
  (env)
fun ARGS_21 (env, MINUS) = 
  (env)
fun ARGS_23 (LP, env) = 
  (env)
fun ARGS_24 (v) = 
  (v)
fun ARGS_27 (v, ID, SEMI, TIPO) = 
  (print (Int.toString (AtomMap.numItems v)); insere(v,Atom.atom ID,Atom.toString(Atom.atom TIPO)))

    end

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)
    structure EBNF = AntlrEBNF(struct
			         type strm = Err.wstream
			         val getSpan = Err.getSpan
			       end)

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
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
fun matchSPACE strm = (case (lex(strm))
 of (Tok.SPACE, span, strm') => ((), span, strm')
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
fun matchMINUS strm = (case (lex(strm))
 of (Tok.MINUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTIMES strm = (case (lex(strm))
 of (Tok.TIMES, span, strm') => ((), span, strm')
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
fun matchNUM strm = (case (lex(strm))
 of (Tok.NUM(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchID strm = (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
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
fun exp_NT (env_RES) (strm) = let
      fun exp_PROD_1 (strm) = let
            val (KW_let_RES, KW_let_SPAN, strm') = matchKW_let(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (exp1_RES, exp1_SPAN, strm') = (exp_NT (UserCode.ARGS_10 (EQ_RES, ID_RES, env_RES, KW_let_RES)))(strm')
            val (KW_in_RES, KW_in_SPAN, strm') = matchKW_in(strm')
            val (exp2_RES, exp2_SPAN, strm') = (exp_NT (UserCode.ARGS_11 (EQ_RES, ID_RES, env_RES, exp1_RES, KW_in_RES, KW_let_RES)))(strm')
            val FULL_SPAN = (#1(KW_let_SPAN), #2(exp2_SPAN))
            in
              (UserCode.exp_PROD_1_ACT (EQ_RES, ID_RES, env_RES, exp1_RES, exp2_RES, KW_in_RES, KW_let_RES, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_2 (strm) = let
            val (addExp_RES, addExp_SPAN, strm') = (addExp_NT (UserCode.ARGS_12 (env_RES)))(strm)
            val FULL_SPAN = (#1(addExp_SPAN), #2(addExp_SPAN))
            in
              ((addExp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => exp_PROD_2(strm)
          | (Tok.NUM(_), _, strm') => exp_PROD_2(strm)
          | (Tok.MINUS, _, strm') => exp_PROD_2(strm)
          | (Tok.LP, _, strm') => exp_PROD_2(strm)
          | (Tok.KW_let, _, strm') => exp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and addExp_NT (env_RES) (strm) = let
      val (multExp_RES, multExp_SPAN, strm') = (multExp_NT (UserCode.ARGS_14 (env_RES)))(strm)
      fun addExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm)
            val (multExp_RES, multExp_SPAN, strm') = (multExp_NT (UserCode.ARGS_15 (env_RES, PLUS_RES, multExp_RES)))(strm')
            val FULL_SPAN = (#1(PLUS_SPAN), #2(multExp_SPAN))
            in
              ((multExp_RES), FULL_SPAN, strm')
            end
      fun addExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.PLUS, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(addExp_PROD_1_SUBRULE_1_PRED, addExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(multExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.addExp_PROD_1_ACT (SR_RES, env_RES, multExp_RES, SR_SPAN : (Lex.pos * Lex.pos), multExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and multExp_NT (env_RES) (strm) = let
      val (prefixExp_RES, prefixExp_SPAN, strm') = (prefixExp_NT (UserCode.ARGS_17 (env_RES)))(strm)
      fun multExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
            val (prefixExp_RES, prefixExp_SPAN, strm') = (prefixExp_NT (UserCode.ARGS_18 (env_RES, TIMES_RES, prefixExp_RES)))(strm')
            val FULL_SPAN = (#1(TIMES_SPAN), #2(prefixExp_SPAN))
            in
              ((prefixExp_RES), FULL_SPAN, strm')
            end
      fun multExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.TIMES, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(multExp_PROD_1_SUBRULE_1_PRED, multExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(prefixExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.multExp_PROD_1_ACT (SR_RES, env_RES, prefixExp_RES, SR_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and prefixExp_NT (env_RES) (strm) = let
      fun prefixExp_PROD_1 (strm) = let
            val (atomicExp_RES, atomicExp_SPAN, strm') = (atomicExp_NT (UserCode.ARGS_19 (env_RES)))(strm)
            val FULL_SPAN = (#1(atomicExp_SPAN), #2(atomicExp_SPAN))
            in
              ((atomicExp_RES), FULL_SPAN, strm')
            end
      fun prefixExp_PROD_2 (strm) = let
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
            val (prefixExp_RES, prefixExp_SPAN, strm') = (prefixExp_NT (UserCode.ARGS_21 (env_RES, MINUS_RES)))(strm')
            val FULL_SPAN = (#1(MINUS_SPAN), #2(prefixExp_SPAN))
            in
              (UserCode.prefixExp_PROD_2_ACT (env_RES, MINUS_RES, prefixExp_RES, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.MINUS, _, strm') => prefixExp_PROD_2(strm)
          | (Tok.ID(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.NUM(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.LP, _, strm') => prefixExp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and atomicExp_NT (env_RES) (strm) = let
      fun atomicExp_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.atomicExp_PROD_1_ACT (ID_RES, env_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun atomicExp_PROD_2 (strm) = let
            val (NUM_RES, NUM_SPAN, strm') = matchNUM(strm)
            val FULL_SPAN = (#1(NUM_SPAN), #2(NUM_SPAN))
            in
              ((NUM_RES), FULL_SPAN, strm')
            end
      fun atomicExp_PROD_3 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_RES, exp_SPAN, strm') = (exp_NT (UserCode.ARGS_23 (LP_RES, env_RES)))(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              ((exp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => atomicExp_PROD_3(strm)
          | (Tok.ID(_), _, strm') => atomicExp_PROD_1(strm)
          | (Tok.NUM(_), _, strm') => atomicExp_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun commands_NT (v_RES, ps_RES) (strm) = let
      fun commands_PROD_1 (strm) = let
            val (KW_Print_RES, KW_Print_SPAN, strm') = matchKW_Print(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (STR_RES, STR_SPAN, strm') = matchSTR(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_Print_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.commands_PROD_1_ACT (v_RES, LP_RES, RP_RES, ps_RES, STR_RES, KW_Print_RES, SEMI_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), STR_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun commands_PROD_2 (strm) = let
            val (KW_Print_RES, KW_Print_SPAN, strm') = matchKW_Print(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_Print_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.commands_PROD_2_ACT (v_RES, ID_RES, LP_RES, RP_RES, ps_RES, KW_Print_RES, SEMI_RES, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_Print, _, strm') =>
              (case (lex(strm'))
               of (Tok.LP, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.ID(_), _, strm') => commands_PROD_2(strm)
                      | (Tok.STR(_), _, strm') => commands_PROD_1(strm)
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun variables_NT (v_RES) (strm) = let
      fun variables_PROD_1 (strm) = let
            val (declaration_RES, declaration_SPAN, strm') = (declaration_NT (UserCode.ARGS_24 (v_RES)))(strm)
            val FULL_SPAN = (#1(declaration_SPAN), #2(declaration_SPAN))
            in
              ((declaration_RES), FULL_SPAN, strm')
            end
      fun variables_PROD_2 (strm) = let
            val (KW_endvars_RES, KW_endvars_SPAN, strm') = matchKW_endvars(strm)
            val FULL_SPAN = (#1(KW_endvars_SPAN), #2(KW_endvars_SPAN))
            in
              (UserCode.variables_PROD_2_ACT (v_RES, KW_endvars_RES, KW_endvars_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_endvars, _, strm') => variables_PROD_2(strm)
          | (Tok.TIPO(_), _, strm') => variables_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and declaration_NT (v_RES) (strm) = let
      val (TIPO_RES, TIPO_SPAN, strm') = matchTIPO(strm)
      val (ID_RES, ID_SPAN, strm') = matchID(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val (variables_RES, variables_SPAN, strm') = (variables_NT (UserCode.ARGS_27 (v_RES, ID_RES, SEMI_RES, TIPO_RES)))(strm')
      val FULL_SPAN = (#1(TIPO_SPAN), #2(variables_SPAN))
      in
        (UserCode.declaration_PROD_1_ACT (v_RES, ID_RES, SEMI_RES, TIPO_RES, variables_RES, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TIPO_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun program_NT (env_RES, v_RES, ps_RES) (strm) = let
      val (KW_title_RES, KW_title_SPAN, strm') = matchKW_title(strm)
      val (STR_RES, STR_SPAN, strm') = matchSTR(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val (KW_variables_RES, KW_variables_SPAN, strm') = matchKW_variables(strm')
      val (variables_RES, variables_SPAN, strm') = (variables_NT (UserCode.ARGS_4 (v_RES, ps_RES, STR_RES, env_RES, SEMI_RES, KW_title_RES, KW_variables_RES)))(strm')
      val (KW_comands_RES, KW_comands_SPAN, strm') = matchKW_comands(strm')
      val (commands_RES, commands_SPAN, strm') = (commands_NT (UserCode.ARGS_5 (v_RES, ps_RES, STR_RES, env_RES, SEMI_RES, KW_title_RES, KW_variables_RES, variables_RES, KW_comands_RES)))(strm')
      val (exp_RES, exp_SPAN, strm') = (exp_NT (UserCode.ARGS_6 (v_RES, ps_RES, STR_RES, env_RES, commands_RES, SEMI_RES, KW_title_RES, KW_variables_RES, variables_RES, KW_comands_RES)))(strm')
      val FULL_SPAN = (#1(KW_title_SPAN), #2(exp_SPAN))
      in
        (UserCode.program_PROD_1_ACT (v_RES, ps_RES, STR_RES, env_RES, exp_RES, commands_RES, SEMI_RES, KW_title_RES, KW_variables_RES, variables_RES, KW_comands_RES, STR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), commands_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_title_SPAN : (Lex.pos * Lex.pos), KW_variables_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), KW_comands_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
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
