exception NothingFound

structure DarwinTest =
struct
	open Grammar
	open ParseTree
	structure CP = DarwinParseFn(DarwinLexer)

	fun correctGrammar(If(_,c1::cl,c2::cr)::r) = List.length (c1::cl) + List.length (c2::cr) + correctGrammar(cl) + correctGrammar(cr) + correctGrammar(r)
	  | correctGrammar(If(_,c1::cs,nil)::r) = List.length (c1::cs) + correctGrammar(cs) + correctGrammar(r)
	  | correctGrammar(If(_,nil,c2::cs)::r) = List.length (c2::cs) + correctGrammar(cs) + correctGrammar(r)
	  | correctGrammar(c::r)  = correctGrammar(r)
	  | correctGrammar _ = 0

	fun exnToString(e) = "[" ^ (exnName e) ^ " " ^ (exnMessage e) ^ "]"

	fun darwin instrm =
		let
			val sm = AntlrStreamPos.mkSourcemap()
			val lex = DarwinLexer.lex sm
			val strm = DarwinLexer.streamifyInstream instrm
			val _ = print "      _                     _       \n"
			val _ = print "   __| | __ _ _ ____      _(_)_ __  \n"
			val _ = print "  / _` |/ _` | '__\\ \\ /\\ / / | '_ \\ \n"
			val _ = print " | (_| | (_| | |   \\ V  V /| | | | |\n"
			val _ = print "  \\__,_|\\__,_|_|    \\_/\\_/ |_|_| |_|\n"
			val _ = print "                                    \n"
			val _ = print "        Interpreting code...        \n"
			val _ = print "                                    \n"

			val (r, strm', errs, {tree=tr,vars=vs,ts=tps}) = CP.parse lex strm
			fun doErr err = print ("Syntax error " ^
			    AntlrRepair.repairToString DarwinTokens.toString sm err ^ "\n")
			val _ = app doErr errs
		in
			print (Int.toString(correctGrammar tr));
			(case r of SOME r2 => ParseTree.interpret(r2,vs,tps) | NONE => raise NothingFound);
			r
		end

	fun main (prog_name) =
    	let
      		val _ = darwin (TextIO.openIn "lol")
    	in
      		1
    	end
    	handle e => (print "Error\n";exnToString e; 43)


end
