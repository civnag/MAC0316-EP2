structure DarwinTest =
struct
	open Grammar
	structure CP = DarwinParseFn(DarwinLexer)
	
	fun tok2s (DarwinTokens.ID s) = s
		| tok2s (DarwinTokens.NUM n) = Int.toString n
		| tok2s tok = DarwinTokens.toString tok

	fun calc instrm = 
		let
			val sm = AntlrStreamPos.mkSourcemap()
			val lex = DarwinLexer.lex sm
			val strm = DarwinLexer.streamifyInstream instrm
			val (r, strm', errs) = CP.parse lex (AtomMap.empty,AtomMap.empty,nil) strm
			val _ = List.app print (Option.valOf r)
		in 
			print (String.concatWith "\n"
				(List.map (AntlrRepair.repairToString tok2s sm)
				errs));
			r
		end

	fun main (prog_name, args) =
    	let
      		val _ = calc (TextIO.stdIn)
    	in
      		1
    	end
end