structure DarwinTest =
struct
	open Grammar
	open ParseTree
	structure CP = DarwinParseFn(DarwinLexer)

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
			ParseTree.interpret(tr,vs,tps);
			r
		end

	fun main (prog_name) =
    	let
      		val _ = darwin (TextIO.openIn "lol")
    	in
      		1
    	end
end
