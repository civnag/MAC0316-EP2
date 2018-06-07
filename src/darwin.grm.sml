structure 
DarwinTokens = struct

    datatype token = EOF
      | KW_terminate
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

    val allToks = [EOF, KW_terminate, KW_endvars, KW_Print, KW_comands, SEMI, KW_variables, SPACE, RP, LP, MINUS, TIMES, PLUS, EQ, KW_title, KW_in, KW_let]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (KW_terminate) => "terminate"
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
  | (KW_terminate) => true
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
    
    fun getInt x = (Grammar.extractInt x)
    
    fun getVar v = AtomMap.appi (fn (k,w) => print (
        let val _ = print(Atom.toString k) 
            val _ = "oi"
            val _ = print (Grammar.show w)
        in 
            ""
        end)) v


fun program_PROD_1_ACT (d, STR, commands, SEMI, KW_title, KW_variables, variables, KW_comands, STR_SPAN : (Lex.pos * Lex.pos), commands_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_title_SPAN : (Lex.pos * Lex.pos), KW_variables_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), KW_comands_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v) = 
  ( )
fun commands_PROD_1_ACT (SR1, SR2, SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v) = 
  ( )
fun assign_PROD_1_ACT (EQ, ID, SEMI, expr, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v) = 
  ( 
        let 
            val _ = print ("assign\n")
        in
            v := Grammar.updateHt(!v,Atom.atom ID,expr)
        end)
fun expr_PROD_1_ACT (exp_arit, exp_arit_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v) = 
  ( Grammar.Int_ exp_arit)
fun prints_PROD_1_ACT (LP, RP, STR, KW_Print, SEMI, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), STR_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v) = 
  (  ps := (STR::(!ps)) )
fun prints_PROD_2_ACT (ID, LP, RP, KW_Print, SEMI, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v) = 
  ( 
        let 
            val _ = print "print\n"
            val k = Grammar.show (valOf (AtomMap.find (!v, Atom.atom ID)))
        in 
            ps := k::(!ps)
        end
      )
fun addExp_PROD_1_ACT (SR, multExp, SR_SPAN : (Lex.pos * Lex.pos), multExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v) = 
  (  List.foldr op+ 0 (multExp::SR) )
fun multExp_PROD_1_ACT (SR, prefixExp, SR_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v) = 
  (  List.foldr op* 1 (prefixExp::SR) )
fun prefixExp_PROD_2_ACT (MINUS, prefixExp, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v) = 
  (  ~prefixExp )
fun atomicExp_PROD_1_ACT (ID, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v) = 
  (  getInt (valOf(AtomMap.find (!v, Atom.atom ID))) )
fun declaration_PROD_1_ACT (ID, SEMI, TIPO, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TIPO_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps, v) = 
  ( v:=insere(!v,Atom.atom ID,Atom.toString(Atom.atom TIPO)))
fun mkps_REFC() : (string list) ref = ref ( nil)
fun mkv_REFC() : ((Grammar.tipo_primitivo) AtomMap.map) ref = ref ( AtomMap.empty)

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
fun getS() = {ps = !ps_REFC, v = !v_REFC}
fun putS{ps, v} = (ps_REFC := ps; v_REFC := v)
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
fun exp_arit_NT (strm) = let
      val (addExp_RES, addExp_SPAN, strm') = addExp_NT(strm)
      val FULL_SPAN = (#1(addExp_SPAN), #2(addExp_SPAN))
      in
        ((addExp_RES), FULL_SPAN, strm')
      end
and addExp_NT (strm) = let
      val (multExp_RES, multExp_SPAN, strm') = multExp_NT(strm)
      fun addExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm)
            val (multExp_RES, multExp_SPAN, strm') = multExp_NT(strm')
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
        (UserCode.addExp_PROD_1_ACT (SR_RES, multExp_RES, SR_SPAN : (Lex.pos * Lex.pos), multExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC),
          FULL_SPAN, strm')
      end
and multExp_NT (strm) = let
      val (prefixExp_RES, prefixExp_SPAN, strm') = prefixExp_NT(strm)
      fun multExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
            val (prefixExp_RES, prefixExp_SPAN, strm') = prefixExp_NT(strm')
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
        (UserCode.multExp_PROD_1_ACT (SR_RES, prefixExp_RES, SR_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC),
          FULL_SPAN, strm')
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
              (UserCode.prefixExp_PROD_2_ACT (MINUS_RES, prefixExp_RES, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC),
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
and atomicExp_NT (strm) = let
      fun atomicExp_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              (UserCode.atomicExp_PROD_1_ACT (ID_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC),
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
            val (exp_arit_RES, exp_arit_SPAN, strm') = exp_arit_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              ((exp_arit_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => atomicExp_PROD_3(strm)
          | (Tok.ID(_), _, strm') => atomicExp_PROD_1(strm)
          | (Tok.NUM(_), _, strm') => atomicExp_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun expr_NT (strm) = let
      val (exp_arit_RES, exp_arit_SPAN, strm') = exp_arit_NT(strm)
      val FULL_SPAN = (#1(exp_arit_SPAN), #2(exp_arit_SPAN))
      in
        (UserCode.expr_PROD_1_ACT (exp_arit_RES, exp_arit_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC),
          FULL_SPAN, strm')
      end
fun assign_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
      val (expr_RES, expr_SPAN, strm') = expr_NT(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(ID_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.assign_PROD_1_ACT (EQ_RES, ID_RES, SEMI_RES, expr_RES, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), expr_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC),
          FULL_SPAN, strm')
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
              (UserCode.prints_PROD_1_ACT (LP_RES, RP_RES, STR_RES, KW_Print_RES, SEMI_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), STR_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC),
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
              (UserCode.prints_PROD_2_ACT (ID_RES, LP_RES, RP_RES, KW_Print_RES, SEMI_RES, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC),
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
      val (SR1_RES, SR1_SPAN, strm') = let
      fun commands_PROD_1_SUBRULE_1_NT (strm) = let
            fun commands_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (prints_RES, prints_SPAN, strm') = prints_NT(strm)
                  val FULL_SPAN = (#1(prints_SPAN), #2(prints_SPAN))
                  in
                    ((prints_RES), FULL_SPAN, strm')
                  end
            fun commands_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (assign_RES, assign_SPAN, strm') = assign_NT(strm)
                  val FULL_SPAN = (#1(assign_SPAN), #2(assign_SPAN))
                  in
                    ((assign_RES), FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.ID(_), _, strm') =>
                    commands_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.KW_Print, _, strm') =>
                    commands_PROD_1_SUBRULE_1_PROD_1(strm)
                | _ => fail()
              (* end case *))
            end
      in
        commands_PROD_1_SUBRULE_1_NT(strm)
      end
      val (SR2_RES, SR2_SPAN, strm') = let
      fun commands_PROD_1_SUBRULE_2_NT (strm) = let
            fun commands_PROD_1_SUBRULE_2_PROD_1 (strm) = let
                  val (commands_RES, commands_SPAN, strm') = commands_NT(strm)
                  val FULL_SPAN = (#1(commands_SPAN), #2(commands_SPAN))
                  in
                    ((commands_RES), FULL_SPAN, strm')
                  end
            fun commands_PROD_1_SUBRULE_2_PROD_2 (strm) = let
                  val (KW_terminate_RES, KW_terminate_SPAN, strm') = matchKW_terminate(strm)
                  val FULL_SPAN = (#1(KW_terminate_SPAN),
                    #2(KW_terminate_SPAN))
                  in
                    ((), FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.KW_terminate, _, strm') =>
                    commands_PROD_1_SUBRULE_2_PROD_2(strm)
                | (Tok.ID(_), _, strm') =>
                    commands_PROD_1_SUBRULE_2_PROD_1(strm)
                | (Tok.KW_Print, _, strm') =>
                    commands_PROD_1_SUBRULE_2_PROD_1(strm)
                | _ => fail()
              (* end case *))
            end
      in
        commands_PROD_1_SUBRULE_2_NT(strm')
      end
      val FULL_SPAN = (#1(SR1_SPAN), #2(SR2_SPAN))
      in
        (UserCode.commands_PROD_1_ACT (SR1_RES, SR2_RES, SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC),
          FULL_SPAN, strm')
      end
fun declaration_NT (strm) = let
      val (TIPO_RES, TIPO_SPAN, strm') = matchTIPO(strm)
      val (ID_RES, ID_SPAN, strm') = matchID(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(TIPO_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.declaration_PROD_1_ACT (ID_RES, SEMI_RES, TIPO_RES, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TIPO_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC),
          FULL_SPAN, strm')
      end
fun variables_NT (strm) = let
      val (declaration_RES, declaration_SPAN, strm') = declaration_NT(strm)
      val (SR_RES, SR_SPAN, strm') = let
      fun variables_PROD_1_SUBRULE_1_NT (strm) = let
            fun variables_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                  val (declaration_RES, declaration_SPAN, strm') = declaration_NT(strm)
                  val FULL_SPAN = (#1(declaration_SPAN), #2(declaration_SPAN))
                  in
                    ((declaration_RES), FULL_SPAN, strm')
                  end
            fun variables_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                  val (KW_endvars_RES, KW_endvars_SPAN, strm') = matchKW_endvars(strm)
                  val FULL_SPAN = (#1(KW_endvars_SPAN), #2(KW_endvars_SPAN))
                  in
                    ((), FULL_SPAN, strm')
                  end
            in
              (case (lex(strm))
               of (Tok.KW_endvars, _, strm') =>
                    variables_PROD_1_SUBRULE_1_PROD_2(strm)
                | (Tok.TIPO(_), _, strm') =>
                    variables_PROD_1_SUBRULE_1_PROD_1(strm)
                | _ => fail()
              (* end case *))
            end
      in
        variables_PROD_1_SUBRULE_1_NT(strm')
      end
      val FULL_SPAN = (#1(declaration_SPAN), #2(SR_SPAN))
      in
        ((declaration_RES, SR_RES), FULL_SPAN, strm')
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
        (UserCode.program_PROD_1_ACT (d_RES, STR_RES, commands_RES, SEMI_RES, KW_title_RES, KW_variables_RES, variables_RES, KW_comands_RES, STR_SPAN : (Lex.pos * Lex.pos), commands_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_title_SPAN : (Lex.pos * Lex.pos), KW_variables_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), KW_comands_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), ps_REFC, v_REFC),
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
