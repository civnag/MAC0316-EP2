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
#[([(0w0,0w8,1),
(0w11,0w31,1),
(0w35,0w37,1),
(0w39,0w39,1),
(0w44,0w44,1),
(0w58,0w58,1),
(0w63,0w64,1),
(0w91,0w96,1),
(0w123,0w123,1),
(0w125,0w2147483647,1),
(0w9,0w10,2),
(0w32,0w32,2),
(0w33,0w33,3),
(0w34,0w34,4),
(0w38,0w38,5),
(0w40,0w40,6),
(0w41,0w41,7),
(0w42,0w42,8),
(0w43,0w43,9),
(0w45,0w45,10),
(0w46,0w46,11),
(0w47,0w47,12),
(0w48,0w57,13),
(0w59,0w59,14),
(0w60,0w60,15),
(0w61,0w61,16),
(0w62,0w62,17),
(0w65,0w90,18),
(0w97,0w97,18),
(0w100,0w100,18),
(0w103,0w104,18),
(0w106,0w107,18),
(0w109,0w111,18),
(0w113,0w114,18),
(0w117,0w117,18),
(0w119,0w122,18),
(0w98,0w98,19),
(0w99,0w99,20),
(0w101,0w101,21),
(0w102,0w102,22),
(0w105,0w105,23),
(0w108,0w108,24),
(0w112,0w112,25),
(0w115,0w115,26),
(0w116,0w116,27),
(0w118,0w118,28),
(0w124,0w124,29)], []), ([], [34]), ([], [32, 34]), ([(0w61,0w61,157)], [26, 34]), ([(0w65,0w90,155),
(0w97,0w122,155)], [34]), ([(0w38,0w38,154)], [34]), ([], [21, 34]), ([], [22, 34]), ([], [19, 34]), ([], [17, 34]), ([], [18, 34]), ([], [23, 34]), ([], [20, 34]), ([(0w34,0w34,148),
(0w46,0w46,148),
(0w48,0w57,149)], [11, 34]), ([], [16, 34]), ([(0w61,0w61,147)], [34]), ([(0w61,0w61,146)], [14, 34]), ([(0w61,0w61,145)], [34]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w122,31)], [9, 34]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w110,31),
(0w112,0w122,31),
(0w111,0w111,140)], [9, 34]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w110,31),
(0w112,0w122,31),
(0w111,0w111,133)], [9, 34]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w109,31),
(0w111,0w122,31),
(0w110,0w110,121)], [9, 34]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w98,0w107,31),
(0w109,0w122,31),
(0w97,0w97,116),
(0w108,0w108,117)], [9, 34]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w109,31),
(0w111,0w122,31),
(0w110,0w110,115)], [9, 34]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w100,31),
(0w102,0w122,31),
(0w101,0w101,113)], [9, 34]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w113,31),
(0w115,0w122,31),
(0w114,0w114,109)], [9, 34]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w98,0w115,31),
(0w117,0w122,31),
(0w97,0w97,79),
(0w116,0w116,80)], [9, 34]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w100,31),
(0w102,0w104,31),
(0w106,0w113,31),
(0w115,0w116,31),
(0w118,0w122,31),
(0w101,0w101,40),
(0w105,0w105,41),
(0w114,0w114,42),
(0w117,0w117,43)], [9, 34]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w98,0w122,31),
(0w97,0w97,32)], [9, 34]), ([(0w124,0w124,30)], [34]), ([], [25]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w122,31)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w113,31),
(0w115,0w122,31),
(0w114,0w114,33)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w104,31),
(0w106,0w122,31),
(0w105,0w105,34)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w98,0w122,31),
(0w97,0w97,35)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w97,31),
(0w99,0w122,31),
(0w98,0w98,36)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w107,31),
(0w109,0w122,31),
(0w108,0w108,37)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w100,31),
(0w102,0w122,31),
(0w101,0w101,38)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w114,31),
(0w116,0w122,31),
(0w115,0w115,39)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w122,31)], [1, 9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w113,31),
(0w115,0w122,31),
(0w114,0w114,72)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w115,31),
(0w117,0w122,31),
(0w116,0w116,69)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w116,31),
(0w118,0w122,31),
(0w117,0w117,67)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w111,31),
(0w113,0w122,31),
(0w112,0w112,44)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w107,31),
(0w109,0w122,31),
(0w108,0w108,45)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w100,31),
(0w102,0w122,31),
(0w101,0w101,46)], [9]), ([(0w40,0w40,47),
(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w122,31)], [9]), ([(0w98,0w98,48),
(0w102,0w102,49),
(0w105,0w105,50),
(0w115,0w115,51)], []), ([(0w111,0w111,62)], []), ([(0w108,0w108,60)], []), ([(0w110,0w110,59)], []), ([(0w116,0w116,52)], []), ([(0w114,0w114,53)], []), ([(0w105,0w105,54)], []), ([(0w110,0w110,55)], []), ([(0w103,0w103,56)], []), ([(0w41,0w41,57),
(0w44,0w44,58),
(0w98,0w98,48),
(0w102,0w102,49),
(0w105,0w105,50),
(0w115,0w115,51)], []), ([], [8]), ([(0w41,0w41,57),
(0w98,0w98,48),
(0w102,0w102,49),
(0w105,0w105,50),
(0w115,0w115,51)], []), ([(0w116,0w116,56)], []), ([(0w111,0w111,61)], []), ([(0w97,0w97,59)], []), ([(0w111,0w111,63)], []), ([(0w108,0w108,64)], []), ([(0w101,0w101,65)], []), ([(0w97,0w97,66)], []), ([(0w110,0w110,56)], []), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w100,31),
(0w102,0w122,31),
(0w101,0w101,68)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w122,31)], [9, 13]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w107,31),
(0w109,0w122,31),
(0w108,0w108,70)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w100,31),
(0w102,0w122,31),
(0w101,0w101,71)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w122,31)], [2, 9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w108,31),
(0w110,0w122,31),
(0w109,0w109,73)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w104,31),
(0w106,0w122,31),
(0w105,0w105,74)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w109,31),
(0w111,0w122,31),
(0w110,0w110,75)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w98,0w122,31),
(0w97,0w97,76)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w115,31),
(0w117,0w122,31),
(0w116,0w116,77)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w100,31),
(0w102,0w122,31),
(0w101,0w101,78)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w122,31)], [9, 33]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w108,31),
(0w110,0w122,31),
(0w109,0w109,85)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w113,31),
(0w115,0w122,31),
(0w114,0w114,81)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w104,31),
(0w106,0w122,31),
(0w105,0w105,82)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w109,31),
(0w111,0w122,31),
(0w110,0w110,83)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w102,31),
(0w104,0w122,31),
(0w103,0w103,84)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w122,31)], [7, 9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w111,31),
(0w113,0w122,31),
(0w112,0w112,86)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w107,31),
(0w109,0w122,31),
(0w108,0w108,87)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w100,31),
(0w102,0w122,31),
(0w101,0w101,88)], [9]), ([(0w32,0w32,89),
(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w122,31)], [9]), ([(0w111,0w111,90)], []), ([(0w102,0w102,91)], []), ([(0w32,0w32,92)], []), ([(0w98,0w98,93),
(0w102,0w102,94),
(0w105,0w105,95),
(0w115,0w115,96)], []), ([(0w111,0w111,104)], []), ([(0w108,0w108,102)], []), ([(0w110,0w110,101)], []), ([(0w116,0w116,97)], []), ([(0w114,0w114,98)], []), ([(0w105,0w105,99)], []), ([(0w110,0w110,100)], []), ([(0w103,0w103,57)], []), ([(0w116,0w116,57)], []), ([(0w111,0w111,103)], []), ([(0w97,0w97,101)], []), ([(0w111,0w111,105)], []), ([(0w108,0w108,106)], []), ([(0w101,0w101,107)], []), ([(0w97,0w97,108)], []), ([(0w110,0w110,57)], []), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w104,31),
(0w106,0w122,31),
(0w105,0w105,110)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w109,31),
(0w111,0w122,31),
(0w110,0w110,111)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w115,31),
(0w117,0w122,31),
(0w116,0w116,112)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w122,31)], [4, 9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w115,31),
(0w117,0w122,31),
(0w116,0w116,114)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w122,31)], [0, 9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w115,31),
(0w117,0w122,31),
(0w116,0w116,84)], [6, 9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w107,31),
(0w109,0w122,31),
(0w108,0w108,120)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w110,31),
(0w112,0w122,31),
(0w111,0w111,118)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w98,0w122,31),
(0w97,0w97,119)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w115,31),
(0w117,0w122,31),
(0w116,0w116,84)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w114,31),
(0w116,0w122,31),
(0w115,0w115,67)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w99,31),
(0w101,0w122,31),
(0w100,0w100,122)], [9]), ([(0w32,0w32,123),
(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w122,31)], [9]), ([(0w118,0w118,124)], []), ([(0w97,0w97,125)], []), ([(0w114,0w114,126)], []), ([(0w105,0w105,127)], []), ([(0w97,0w97,128)], []), ([(0w98,0w98,129)], []), ([(0w108,0w108,130)], []), ([(0w101,0w101,131)], []), ([(0w115,0w115,132)], []), ([], [5]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w108,31),
(0w110,0w122,31),
(0w109,0w109,134)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w108,31),
(0w110,0w122,31),
(0w109,0w109,135)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w98,0w122,31),
(0w97,0w97,136)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w109,31),
(0w111,0w122,31),
(0w110,0w110,137)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w99,31),
(0w101,0w122,31),
(0w100,0w100,138)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w114,31),
(0w116,0w122,31),
(0w115,0w115,139)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w122,31)], [3, 9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w110,31),
(0w112,0w122,31),
(0w111,0w111,141)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w107,31),
(0w109,0w122,31),
(0w108,0w108,142)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w100,31),
(0w102,0w122,31),
(0w101,0w101,143)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w98,0w122,31),
(0w97,0w97,144)], [9]), ([(0w48,0w57,31),
(0w65,0w90,31),
(0w97,0w109,31),
(0w111,0w122,31),
(0w110,0w110,84)], [9]), ([], [27, 29]), ([], [15]), ([], [28, 30]), ([(0w48,0w57,150)], []), ([(0w34,0w34,148),
(0w46,0w46,148),
(0w48,0w57,149)], [11]), ([(0w48,0w57,151),
(0w69,0w69,152),
(0w101,0w101,152)], [12]), ([(0w48,0w57,151)], [12]), ([(0w48,0w57,153)], []), ([(0w48,0w57,153)], [12]), ([], [24]), ([(0w34,0w34,156),
(0w48,0w57,155),
(0w65,0w90,155),
(0w97,0w122,155)], []), ([], [10]), ([], [31])]
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
        yystrm := strm;   T.TIPO yytext 
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.ID yytext 
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.STR yytext
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.NUM (valOf (Int.fromString yytext)) 
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.REAL (valOf (Real.fromString yytext)) 
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.BOOL (valOf (Bool.fromString yytext)) 
      end
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;   T.EQ )
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;   T.EEQ )
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;   T.SEMI)
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;   T.PLUS )
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;   T.MINUS )
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;   T.TIMES )
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;   T.DIV )
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LP )
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;   T.RP )
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;   T.DOT )
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;   T.AND )
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;   T.OR )
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;   T.NOT )
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;   T.GEQ )
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LEQ )
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;   T.GT )
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LT )
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;   T.NEQ )
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;   continue() )
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
        T.KW_terminate )
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         print (concat ["Unexpected character: '", yytext,
			           "'\n"]); continue()
      end
val yyactTable = Vector.fromList([yyAction0, yyAction1, yyAction2, yyAction3,
  yyAction4, yyAction5, yyAction6, yyAction7, yyAction8, yyAction9, yyAction10,
  yyAction11, yyAction12, yyAction13, yyAction14, yyAction15, yyAction16,
  yyAction17, yyAction18, yyAction19, yyAction20, yyAction21, yyAction22,
  yyAction23, yyAction24, yyAction25, yyAction26, yyAction27, yyAction28,
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34])
in
  if ULexBuffer.eof(!(yystrm))
    then let
      val yycolno = ref(yygetcolNo(!(yystrm)))
      val yylineno = ref(yygetlineNo(!(yystrm)))
      in
        (case (!(yyss))
         of _ => (UserDeclarations.eof())
        (* end case *))
      end
    else (case (!(yyss))
       of INITIAL => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
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
