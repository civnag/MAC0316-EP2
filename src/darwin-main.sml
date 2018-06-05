structure DarwinTest =
struct
	open Grammar
	structure CP = DarwinParseFn(DarwinLexer)
	
	fun tok2s (DarwinTokens.ID s) = s
		| tok2s (DarwinTokens.NUM n) = Int.toString n
		| tok2s tok = DarwinTokens.toString tok

	fun darwin instrm = 
		let
			val sm = AntlrStreamPos.mkSourcemap()
			val lex = DarwinLexer.lex sm
			val strm = DarwinLexer.streamifyInstream instrm
			val _ = print "Interpreting Darwin..."
			val (r, strm', errs) = CP.parse lex (AtomMap.empty,AtomMap.empty,nil) strm
			fun doErr err = print ("Syntax error " ^ 
			    AntlrRepair.repairToString DarwinTokens.toString sm err ^ "\n")
			val _ = app doErr errs
		in 
			case r of
				NONE => print "Nothing"
				| SOME s => List.app print (List.rev s)
		end

	fun main (prog_name, args) =
    	let
      		val _ = darwin (TextIO.openIn "lol")
    	in
      		1
    	end
end