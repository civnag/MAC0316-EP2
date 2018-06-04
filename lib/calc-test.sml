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
			val _ = print (Int.toString (Option.valOf r))
		in 
			1
		end

	fun main (prog_name, args) =
    	let
      		val _ = calc (TextIO.stdIn)
    	in
      		1
    	end
end