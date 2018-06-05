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


fun program_PROD_1_ACT (v, ps, STR, env, commands, SEMI, KW_title, KW_variables, variables, KW_comands, STR_SPAN : (Lex.pos * Lex.pos), commands_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_title_SPAN : (Lex.pos * Lex.pos), KW_variables_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), KW_comands_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( commands)
fun commands_PROD_2_ACT (v, ps, KW_terminate, KW_terminate_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ps)
fun prints_PROD_1_ACT (v, LP, RP, ps, STR, KW_Print, commands, SEMI, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), STR_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), commands_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (  commands )
fun prints_PROD_2_ACT (v, ID, LP, RP, ps, KW_Print, commands, SEMI, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), commands_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( commands)
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
  ( v)
fun declaration_PROD_1_ACT (v, ID, SEMI, TIPO, variables, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), TIPO_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( variables)
fun ARGS_4 (v, ps, STR, env, SEMI, KW_title, KW_variables) = 
  (v)
fun ARGS_5 (v, ps, STR, env, SEMI, KW_title, KW_variables, variables, KW_comands) = 
  (variables,ps)
fun ARGS_6 (v, ps) = 
  (v,ps)
fun ARGS_9 (v, LP, RP, ps, STR, KW_Print, SEMI) = 
  (v,STR::ps)
fun ARGS_11 (v, ID, LP, RP, ps, KW_Print, SEMI) = 
  (
        let 
            val k = Grammar.show (valOf (AtomMap.find (v, Atom.atom ID)))
        in 
            (v,k::ps)
        end
      )
fun ARGS_13 (EQ, ID, env, KW_let) = 
  (env)
fun ARGS_14 (EQ, ID, env, exp1, KW_in, KW_let) = 
  (AtomMap.insert(env, Atom.atom ID, exp1))
fun ARGS_15 (env) = 
  (env)
fun ARGS_18 (env, PLUS, multExp) = 
  (env)
fun ARGS_17 (env) = 
  (env)
fun ARGS_21 (env, TIMES, prefixExp) = 
  (env)
fun ARGS_20 (env) = 
  (env)
fun ARGS_22 (env) = 
  (env)
fun ARGS_24 (env, MINUS) = 
  (env)
fun ARGS_26 (LP, env) = 
  (env)
fun ARGS_27 (v) = 
  (v)
fun ARGS_30 (v, ID, SEMI, TIPO) = 
  (insere(v,Atom.atom ID,Atom.toString(Atom.atom TIPO)))

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
fun commands_NT (v_RES, ps_RES) (strm) = let
      fun commands_PROD_1 (strm) = let
            val (prints_RES, prints_SPAN, strm') = (prints_NT (UserCode.ARGS_6 (v_RES, ps_RES)))(strm)
            val FULL_SPAN = (#1(prints_SPAN), #2(prints_SPAN))
            in
              ((prints_RES), FULL_SPAN, strm')
            end
      fun commands_PROD_2 (strm) = let
            val (KW_terminate_RES, KW_terminate_SPAN, strm') = matchKW_terminate(strm)
            val FULL_SPAN = (#1(KW_terminate_SPAN), #2(KW_terminate_SPAN))
            in
              (UserCode.commands_PROD_2_ACT (v_RES, ps_RES, KW_terminate_RES, KW_terminate_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_terminate, _, strm') => commands_PROD_2(strm)
          | (Tok.KW_Print, _, strm') => commands_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and prints_NT (v_RES, ps_RES) (strm) = let
      fun prints_PROD_1 (strm) = let
            val (KW_Print_RES, KW_Print_SPAN, strm') = matchKW_Print(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (STR_RES, STR_SPAN, strm') = matchSTR(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (commands_RES, commands_SPAN, strm') = (commands_NT (UserCode.ARGS_9 (v_RES, LP_RES, RP_RES, ps_RES, STR_RES, KW_Print_RES, SEMI_RES)))(strm')
            val FULL_SPAN = (#1(KW_Print_SPAN), #2(commands_SPAN))
            in
              (UserCode.prints_PROD_1_ACT (v_RES, LP_RES, RP_RES, ps_RES, STR_RES, KW_Print_RES, commands_RES, SEMI_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), STR_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), commands_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun prints_PROD_2 (strm) = let
            val (KW_Print_RES, KW_Print_SPAN, strm') = matchKW_Print(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (commands_RES, commands_SPAN, strm') = (commands_NT (UserCode.ARGS_11 (v_RES, ID_RES, LP_RES, RP_RES, ps_RES, KW_Print_RES, SEMI_RES)))(strm')
            val FULL_SPAN = (#1(KW_Print_SPAN), #2(commands_SPAN))
            in
              (UserCode.prints_PROD_2_ACT (v_RES, ID_RES, LP_RES, RP_RES, ps_RES, KW_Print_RES, commands_RES, SEMI_RES, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), KW_Print_SPAN : (Lex.pos * Lex.pos), commands_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
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
fun variables_NT (v_RES) (strm) = let
      fun variables_PROD_1 (strm) = let
            val (declaration_RES, declaration_SPAN, strm') = (declaration_NT (UserCode.ARGS_27 (v_RES)))(strm)
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
      val (variables_RES, variables_SPAN, strm') = (variables_NT (UserCode.ARGS_30 (v_RES, ID_RES, SEMI_RES, TIPO_RES)))(strm')
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
      val FULL_SPAN = (#1(KW_title_SPAN), #2(commands_SPAN))
      in
        (UserCode.program_PROD_1_ACT (v_RES, ps_RES, STR_RES, env_RES, commands_RES, SEMI_RES, KW_title_RES, KW_variables_RES, variables_RES, KW_comands_RES, STR_SPAN : (Lex.pos * Lex.pos), commands_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_title_SPAN : (Lex.pos * Lex.pos), KW_variables_SPAN : (Lex.pos * Lex.pos), variables_SPAN : (Lex.pos * Lex.pos), KW_comands_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
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
