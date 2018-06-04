structure CalcTest =
struct
	structure CP = CalcParseFn(CalcLexer)
	
	fun tok2s (CalcTokens.ID s) = s
		| tok2s (CalcTokens.NUM n) = Int.toString n
		| tok2s tok = CalcTokens.toString tok

	fun calc instrm = 
		let
			val sm = AntlrStreamPos.mkSourcemap()
			val lex = CalcLexer.lex sm
			val strm = CalcLexer.streamifyInstream instrm
			val (r, strm', errs) = CP.parse lex AtomMap.empty strm
		in
			print (String.concatWith "\n"
				(List.map (AntlrRepair.repairToString tok2s sm)
				errs));
			r
		end

	fun main (prog_name, args) =
    	let
      		val _ = print "OI"
    	in
      		1
    	end
end