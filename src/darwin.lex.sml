structure DarwinLexer  = struct

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

 
    structure T = DarwinTokens
    type lex_result = T.token
    fun eof() = T.EOF


      end

    local
    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
Vector.fromList []
    fun yystreamify' p input = ULexBuffer.mkStream (p, input)

    fun yystreamifyReader' p readFn strm = let
          val s = ref strm
	  fun iter(strm, n, accum) = 
	        if n > 1024 then (String.implode (rev accum), strm)
		else (case readFn strm
		       of NONE => (String.implode (rev accum), strm)
			| SOME(c, strm') => iter (strm', n+1, c::accum))
          fun input() = let
	        val (data, strm) = iter(!s, 0, [])
	        in
	          s := strm;
		  data
	        end
          in
            yystreamify' p input
          end

    fun yystreamifyInstream' p strm = yystreamify' p (fn ()=>TextIO.input strm)

    fun innerLex 
(yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yysetStrm strm = yystrm := strm
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
	  fun yystreamify input = yystreamify' (yygetPos()) input
	  fun yystreamifyReader readFn strm = yystreamifyReader' (yygetPos()) readFn strm
	  fun yystreamifyInstream strm = yystreamifyInstream' (yygetPos()) strm
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case ULexBuffer.getu strm
                of (SOME (0w10, s')) => 
		     (AntlrStreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
          fun yygetList getc strm = let
            val get1 = UTF8.getu getc
            fun iter (strm, accum) = 
	        (case get1 strm
	          of NONE => rev accum
	           | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
          in
            iter (strm, [])
          end
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yygetList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = AntlrStreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = AntlrStreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    val yylastwasnref = ref (ULexBuffer.lastWasNL (!yystrm))
	    fun continue() = let val yylastwasn = !yylastwasnref in
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_let )
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_variables )
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_title )
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_comands )
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_Print )
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_endvars )
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_in )
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.TIPO yytext 
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.ID yytext 
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.STR yytext
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.NUM (valOf (Int.fromString yytext)) 
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.REAL (valOf (Real.fromString yytext)) 
      end
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;   T.EQ )
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;   T.SEMI)
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;   T.PLUS )
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;   T.MINUS )
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;   T.TIMES )
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;   T.DIV )
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LP )
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;   T.RP )
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;   continue() )
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
        T.KW_terminate )
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         print (concat ["Unexpected character: '", yytext,
			           "'\n"]); continue()
      end
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction8(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction1(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx73
              then yyQ31(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx73
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx65
              then yyQ30(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx65
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6C
              then yyQ29(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6C
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx62
              then yyQ28(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx61
                  then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx62
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx61
                  then yyQ27(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx69
              then yyQ26(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx69
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx72
              then yyQ25(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx72
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx62
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx61
                  then yyQ24(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ23(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ23(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ23(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ23(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ23(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx65
              then yyQ36(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx65
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6C
              then yyQ35(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6C
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx74
              then yyQ34(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx74
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction8(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx65
              then yyQ43(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx65
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx74
              then yyQ42(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx74
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx62
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx61
                  then yyQ41(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6E
              then yyQ40(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx69
              then yyQ39(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx69
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6D
              then yyQ38(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx72
              then yyQ37(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx72
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction8(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ33(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx65
                  then yyQ32(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ23(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ23(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ23(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ23(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ23(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx67
              then yyQ48(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx67
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6E
              then yyQ47(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx69
              then yyQ46(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx69
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx72
              then yyQ45(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx72
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx74
              then yyQ44(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx74
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ23(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ23(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction4(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ23(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ23(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ23(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx74
              then yyQ52(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx74
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6E
              then yyQ51(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx69
              then yyQ50(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx69
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx72
              then yyQ49(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx72
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ23(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ23(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction0(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ23(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ23(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ23(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx74
              then yyQ54(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx74
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx65
              then yyQ53(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx65
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction6(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = 0wx74
              then yyQ48(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp < 0wx74
              then if inp <= 0wx60
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6E
              then yyQ55(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx73
              then yyQ67(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ66(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx6C
              then yyQ65(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx62
              then yyQ64(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ63(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx69
              then yyQ62(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx72
              then yyQ61(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ60(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx76
              then yyQ59(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx3A
              then if inp = 0wx21
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx21
                  then if inp = 0wx20
                      then yyQ58(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyAction8(strm, yyNO_MATCH)
                else if inp <= 0wx2F
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx64
              then yyQ57(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx64
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6E
              then yyQ56(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ23(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ23(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction3(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ23(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ23(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ23(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx73
              then yyQ74(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx73
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx64
              then yyQ73(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx64
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6E
              then yyQ72(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx62
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx61
                  then yyQ71(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6D
              then yyQ70(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6D
              then yyQ69(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6F
              then yyQ68(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6E
              then yyQ48(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx62
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx61
                  then yyQ79(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx65
              then yyQ78(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx65
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6C
              then yyQ77(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6C
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6F
              then yyQ76(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx6F
              then yyQ75(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp <= 0wx60
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction8(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx5A
                  then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ23(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ83(strm', lastMatch)
            else if inp < 0wx65
              then if inp = 0wx2A
                  then yystuck(lastMatch)
                else if inp < 0wx2A
                  then if inp = 0wx23
                      then yystuck(lastMatch)
                    else if inp < 0wx23
                      then if inp = 0wx22
                          then yyQ83(strm', lastMatch)
                          else yystuck(lastMatch)
                    else if inp <= 0wx27
                      then yystuck(lastMatch)
                      else yyQ83(strm', lastMatch)
                else if inp = 0wx3A
                  then yystuck(lastMatch)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yystuck(lastMatch)
                      else yyQ82(strm', lastMatch)
                else if inp = 0wx45
                  then yyQ83(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx6F
              then yystuck(lastMatch)
            else if inp < 0wx6F
              then if inp = 0wx6A
                  then yystuck(lastMatch)
                else if inp < 0wx6A
                  then if inp = 0wx69
                      then yyQ83(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = 0wx6E
                  then yyQ83(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx75
              then yystuck(lastMatch)
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ83(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx7B
              then yyQ83(strm', lastMatch)
            else if inp < 0wx7B
              then yystuck(lastMatch)
            else if inp <= 0wx7D
              then yyQ83(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ82(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ82(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2E
              then yyQ80(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < 0wx2E
              then if inp = 0wx22
                  then yyQ80(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = 0wx30
              then yyQ81(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction10(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ81(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2E
              then yyQ80(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < 0wx2E
              then if inp = 0wx22
                  then yyQ80(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = 0wx30
              then yyQ81(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction10(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ81(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yystuck(lastMatch)
            else if inp < 0wx3A
              then if inp = 0wx23
                  then yystuck(lastMatch)
                else if inp < 0wx23
                  then if inp = 0wx22
                      then yyQ85(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp <= 0wx2F
                  then yystuck(lastMatch)
                  else yyQ84(strm', lastMatch)
            else if inp = 0wx5B
              then yystuck(lastMatch)
            else if inp < 0wx5B
              then if inp <= 0wx40
                  then yystuck(lastMatch)
                  else yyQ84(strm', lastMatch)
            else if inp = 0wx61
              then yyQ84(strm', lastMatch)
            else if inp < 0wx61
              then yystuck(lastMatch)
            else if inp <= 0wx7A
              then yyQ84(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction22(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp <= 0wx40
                  then yyAction22(strm, yyNO_MATCH)
                  else yyQ84(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ84(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction22(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ84(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ1(strm', lastMatch)
            else if inp < 0wx3E
              then if inp = 0wx2B
                  then yyQ7(strm', lastMatch)
                else if inp < 0wx2B
                  then if inp = 0wx22
                      then yyQ3(strm', lastMatch)
                    else if inp < 0wx22
                      then if inp = 0wxB
                          then yyQ1(strm', lastMatch)
                        else if inp < 0wxB
                          then if inp <= 0wx8
                              then yyQ1(strm', lastMatch)
                              else yyQ2(strm', lastMatch)
                        else if inp = 0wx20
                          then yyQ2(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = 0wx29
                      then yyQ5(strm', lastMatch)
                    else if inp < 0wx29
                      then if inp = 0wx28
                          then yyQ4(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                      else yyQ6(strm', lastMatch)
                else if inp = 0wx30
                  then yyQ10(strm', lastMatch)
                else if inp < 0wx30
                  then if inp = 0wx2E
                      then yyQ1(strm', lastMatch)
                    else if inp < 0wx2E
                      then if inp = 0wx2C
                          then yyQ1(strm', lastMatch)
                          else yyQ8(strm', lastMatch)
                      else yyQ9(strm', lastMatch)
                else if inp = 0wx3B
                  then yyQ11(strm', lastMatch)
                else if inp < 0wx3B
                  then if inp = 0wx3A
                      then yyQ1(strm', lastMatch)
                      else yyQ10(strm', lastMatch)
                else if inp = 0wx3C
                  then yyQ1(strm', lastMatch)
                  else yyQ12(strm', lastMatch)
            else if inp = 0wx6A
              then yyQ13(strm', lastMatch)
            else if inp < 0wx6A
              then if inp = 0wx63
                  then yyQ15(strm', lastMatch)
                else if inp < 0wx63
                  then if inp = 0wx5B
                      then yyQ1(strm', lastMatch)
                    else if inp < 0wx5B
                      then if inp <= 0wx40
                          then yyQ1(strm', lastMatch)
                          else yyQ13(strm', lastMatch)
                    else if inp = 0wx61
                      then yyQ13(strm', lastMatch)
                    else if inp = 0wx62
                      then yyQ14(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = 0wx66
                  then yyQ13(strm', lastMatch)
                else if inp < 0wx66
                  then if inp = 0wx64
                      then yyQ13(strm', lastMatch)
                      else yyQ16(strm', lastMatch)
                else if inp = 0wx69
                  then yyQ17(strm', lastMatch)
                  else yyQ13(strm', lastMatch)
            else if inp = 0wx73
              then yyQ20(strm', lastMatch)
            else if inp < 0wx73
              then if inp = 0wx6D
                  then yyQ13(strm', lastMatch)
                else if inp < 0wx6D
                  then if inp = 0wx6C
                      then yyQ18(strm', lastMatch)
                      else yyQ13(strm', lastMatch)
                else if inp = 0wx70
                  then yyQ19(strm', lastMatch)
                  else yyQ13(strm', lastMatch)
            else if inp = 0wx76
              then yyQ22(strm', lastMatch)
            else if inp < 0wx76
              then if inp = 0wx74
                  then yyQ21(strm', lastMatch)
                  else yyQ13(strm', lastMatch)
            else if inp <= 0wx7A
              then yyQ13(strm', lastMatch)
              else yyQ1(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of INITIAL => yyQ0(!(yystrm), yyNO_MATCH)
  (* end case *))
end
end
            and skip() = (yystartPos := yygetPos(); 
			  yylastwasnref := ULexBuffer.lastWasNL (!yystrm);
			  continue())
	    in (continue(), (!yystartPos, yygetPos()), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm 
(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex 
(yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm 
(STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (yystreamify' 0 input, ref NONE), INITIAL)
    fun streamifyReader readFn strm = (STRM (yystreamifyReader' 0 readFn strm, ref NONE), 
				       INITIAL)
    fun streamifyInstream strm = (STRM (yystreamifyInstream' 0 strm, ref NONE), 
				  INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end
