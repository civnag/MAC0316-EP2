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
(0w63,0w64,1),
(0w91,0w96,1),
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
(0w44,0w44,10),
(0w45,0w45,11),
(0w46,0w46,12),
(0w47,0w47,13),
(0w48,0w57,14),
(0w58,0w58,15),
(0w59,0w59,16),
(0w60,0w60,17),
(0w61,0w61,18),
(0w62,0w62,19),
(0w65,0w90,20),
(0w97,0w97,20),
(0w104,0w104,20),
(0w106,0w107,20),
(0w110,0w111,20),
(0w113,0w113,20),
(0w117,0w117,20),
(0w120,0w122,20),
(0w98,0w98,21),
(0w99,0w99,22),
(0w100,0w100,23),
(0w101,0w101,24),
(0w102,0w102,25),
(0w103,0w103,26),
(0w105,0w105,27),
(0w108,0w108,28),
(0w109,0w109,29),
(0w112,0w112,30),
(0w114,0w114,31),
(0w115,0w115,32),
(0w116,0w116,33),
(0w118,0w118,34),
(0w119,0w119,35),
(0w123,0w123,36),
(0w124,0w124,37)], []), ([], [64]), ([], [62, 64]), ([(0w61,0w61,277)], [53, 64]), ([(0w32,0w33,275),
(0w35,0w38,275),
(0w42,0w42,275),
(0w48,0w58,275),
(0w64,0w90,275),
(0w97,0w122,275),
(0w34,0w34,276)], [64]), ([(0w38,0w38,274)], [64]), ([], [48, 64]), ([], [49, 64]), ([], [46, 64]), ([(0w43,0w43,273)], [43, 64]), ([], [60, 64]), ([], [45, 64]), ([], [50, 64]), ([], [47, 64]), ([(0w34,0w34,268),
(0w46,0w46,268),
(0w48,0w57,269)], [34, 64]), ([(0w61,0w61,267)], [64]), ([], [42, 64]), ([(0w61,0w61,266)], [57, 64]), ([(0w61,0w61,265)], [40, 64]), ([(0w61,0w61,264)], [56, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,259)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,235)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,234)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w107,64),
(0w109,0w109,64),
(0w111,0w122,64),
(0w108,0w108,219),
(0w110,0w110,220)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w107,64),
(0w109,0w122,64),
(0w97,0w97,215),
(0w108,0w108,216)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,199)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w101,64),
(0w103,0w109,64),
(0w111,0w122,64),
(0w102,0w102,197),
(0w110,0w110,198)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,182)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,175)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w110,64),
(0w112,0w113,64),
(0w115,0w122,64),
(0w111,0w111,167),
(0w114,0w114,168)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,166)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w115,64),
(0w118,0w122,64),
(0w97,0w97,116),
(0w116,0w116,117),
(0w117,0w117,118)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w103,64),
(0w106,0w110,64),
(0w112,0w113,64),
(0w115,0w122,64),
(0w101,0w101,83),
(0w104,0w104,84),
(0w105,0w105,85),
(0w111,0w111,86),
(0w114,0w114,87)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w110,64),
(0w112,0w122,64),
(0w97,0w97,69),
(0w111,0w111,70)], [32, 64]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w103,64),
(0w105,0w122,64),
(0w104,0w104,65)], [32, 64]), ([(0w34,0w34,39),
(0w48,0w57,40),
(0w102,0w102,41),
(0w116,0w116,42),
(0w125,0w125,43)], [64]), ([(0w124,0w124,38)], [64]), ([], [52]), ([(0w32,0w33,39),
(0w35,0w38,39),
(0w42,0w42,39),
(0w48,0w58,39),
(0w64,0w90,39),
(0w97,0w122,39),
(0w34,0w34,61)], []), ([(0w34,0w34,51),
(0w46,0w46,51),
(0w44,0w44,52),
(0w48,0w57,40),
(0w125,0w125,53)], []), ([(0w97,0w97,49)], []), ([(0w114,0w114,44)], []), ([], [36, 37, 38, 39, 59]), ([(0w117,0w117,45)], []), ([(0w101,0w101,46)], []), ([(0w44,0w44,47),
(0w125,0w125,48)], []), ([(0w102,0w102,41),
(0w116,0w116,42)], []), ([], [38]), ([(0w108,0w108,50)], []), ([(0w115,0w115,45)], []), ([(0w48,0w57,55)], []), ([(0w48,0w57,54)], []), ([], [36]), ([(0w44,0w44,52),
(0w48,0w57,54),
(0w125,0w125,53)], []), ([(0w44,0w44,56),
(0w48,0w57,55),
(0w69,0w69,57),
(0w101,0w101,57),
(0w125,0w125,58)], []), ([(0w48,0w57,60)], []), ([(0w48,0w57,59)], []), ([], [37]), ([(0w44,0w44,56),
(0w48,0w57,59),
(0w125,0w125,58)], []), ([(0w34,0w34,51),
(0w46,0w46,51),
(0w48,0w57,60)], []), ([(0w32,0w33,39),
(0w35,0w38,39),
(0w42,0w42,39),
(0w48,0w58,39),
(0w64,0w90,39),
(0w97,0w122,39),
(0w34,0w34,61),
(0w44,0w44,62),
(0w125,0w125,63)], []), ([(0w34,0w34,39)], []), ([], [39]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,66)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,67)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,68)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [27, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,73)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,71)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w99,64),
(0w101,0w122,64),
(0w100,0w100,72)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [23, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,74)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,75)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w97,64),
(0w99,0w109,64),
(0w111,0w122,64),
(0w98,0w98,76),
(0w110,0w110,77)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,80)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w98,64),
(0w100,0w122,64),
(0w99,0w99,78)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,79)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [12, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,81)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w114,64),
(0w116,0w122,64),
(0w115,0w115,82)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [0, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,109)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,107)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,104)], [32]), ([(0w48,0w57,64),
(0w65,0w69,64),
(0w71,0w72,64),
(0w74,0w82,64),
(0w84,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64),
(0w70,0w70,90),
(0w73,0w73,91),
(0w83,0w83,92)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w116,64),
(0w118,0w122,64),
(0w117,0w117,88)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,89)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [31, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,100)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,98)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,93)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,94)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,95)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,96)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w102,64),
(0w104,0w122,64),
(0w103,0w103,97)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [6, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,99)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [17, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,101)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,102)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,103)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [16, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,105)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,106)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [1, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,108)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [25, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w108,64),
(0w110,0w122,64),
(0w109,0w109,110)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,111)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,112)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,113)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,114)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,115)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [32, 63]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w108,64),
(0w110,0w122,64),
(0w109,0w109,141)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w99,64),
(0w101,0w113,64),
(0w115,0w122,64),
(0w100,0w100,127),
(0w114,0w114,128)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w97,64),
(0w99,0w108,64),
(0w110,0w122,64),
(0w98,0w98,119),
(0w109,0w109,120)], [32]), ([(0w48,0w57,64),
(0w65,0w82,64),
(0w84,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64),
(0w83,0w83,121)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [4, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,122)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w108,64),
(0w110,0w122,64),
(0w109,0w109,123)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w111,64),
(0w113,0w122,64),
(0w112,0w112,124)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,125)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,126)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [18, 32]), ([(0w48,0w57,64),
(0w65,0w67,64),
(0w69,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64),
(0w68,0w68,132)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,129)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,130)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w102,64),
(0w104,0w122,64),
(0w103,0w103,131)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [30, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,133)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w117,64),
(0w119,0w122,64),
(0w118,0w118,134)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,135)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,136)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,137)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,138)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,139)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,140)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [11, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w111,64),
(0w113,0w122,64),
(0w112,0w112,142)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,143)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,144)], [32]), ([(0w32,0w32,145),
(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [32]), ([(0w111,0w111,146)], []), ([(0w102,0w102,147)], []), ([(0w32,0w32,148)], []), ([(0w98,0w98,149),
(0w102,0w102,150),
(0w105,0w105,151),
(0w115,0w115,152)], []), ([(0w111,0w111,161)], []), ([(0w108,0w108,159)], []), ([(0w110,0w110,158)], []), ([(0w116,0w116,153)], []), ([(0w114,0w114,154)], []), ([(0w105,0w105,155)], []), ([(0w110,0w110,156)], []), ([(0w103,0w103,157)], []), ([], [30]), ([(0w116,0w116,157)], []), ([(0w111,0w111,160)], []), ([(0w97,0w97,158)], []), ([(0w111,0w111,162)], []), ([(0w108,0w108,163)], []), ([(0w101,0w101,164)], []), ([(0w97,0w97,165)], []), ([(0w110,0w110,157)], []), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [22, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w118,64),
(0w120,0w122,64),
(0w119,0w119,174)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w110,64),
(0w112,0w122,64),
(0w105,0w105,169),
(0w111,0w111,170)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,172)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w99,64),
(0w101,0w122,64),
(0w100,0w100,171)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [5, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,173)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [3, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [21, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w99,64),
(0w101,0w122,64),
(0w97,0w97,176),
(0w100,0w100,177)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,181)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,178)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,179)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,180)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [10, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [8, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,183)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,184)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,185)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,186)], [32]), ([(0w48,0w57,64),
(0w65,0w81,64),
(0w83,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64),
(0w82,0w82,187)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,188)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w102,64),
(0w104,0w122,64),
(0w103,0w103,189)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,190)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,191)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w114,64),
(0w116,0w122,64),
(0w115,0w115,192)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w114,64),
(0w116,0w122,64),
(0w115,0w115,193)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,194)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,195)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,196)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [19, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [24, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,131)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,200)], [32]), ([(0w48,0w57,64),
(0w65,0w69,64),
(0w71,0w72,64),
(0w74,0w82,64),
(0w84,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64),
(0w70,0w70,201),
(0w73,0w73,202),
(0w83,0w83,203)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,211)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,209)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,204)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,205)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,206)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,207)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w102,64),
(0w104,0w122,64),
(0w103,0w103,208)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [15, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,210)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [14, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,212)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,213)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,214)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [13, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,218)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,217)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,198)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w114,64),
(0w116,0w122,64),
(0w115,0w115,88)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w114,64),
(0w116,0w122,64),
(0w115,0w115,232)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w99,64),
(0w101,0w122,64),
(0w100,0w100,221)], [32]), ([(0w32,0w32,222),
(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [29, 32]), ([(0w118,0w118,223)], []), ([(0w97,0w97,224)], []), ([(0w114,0w114,225)], []), ([(0w105,0w105,226)], []), ([(0w97,0w97,227)], []), ([(0w98,0w98,228)], []), ([(0w108,0w108,229)], []), ([(0w101,0w101,230)], []), ([(0w115,0w115,231)], []), ([], [7]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,233)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [26, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [28, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w108,64),
(0w110,0w113,64),
(0w115,0w117,64),
(0w119,0w122,64),
(0w109,0w109,236),
(0w114,0w114,237),
(0w118,0w118,238)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w108,64),
(0w110,0w122,64),
(0w109,0w109,254)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,246)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,239)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,240)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,241)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,242)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,243)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w98,64),
(0w100,0w122,64),
(0w99,0w99,244)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,245)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [20, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,247)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,248)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,249)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,250)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,251)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,252)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,253)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [9, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,255)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,256)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w99,64),
(0w101,0w122,64),
(0w100,0w100,257)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w114,64),
(0w116,0w122,64),
(0w115,0w115,258)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w122,64)], [2, 32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,260)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,261)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,262)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w98,0w122,64),
(0w97,0w97,263)], [32]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w95,0w95,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,131)], [32]), ([], [54]), ([], [41]), ([], [55]), ([], [61]), ([(0w48,0w57,270)], []), ([(0w34,0w34,268),
(0w46,0w46,268),
(0w48,0w57,269)], [34]), ([(0w48,0w57,270),
(0w69,0w69,271),
(0w101,0w101,271)], [35]), ([(0w48,0w57,272)], []), ([(0w48,0w57,272)], [35]), ([], [44]), ([], [51]), ([(0w32,0w33,275),
(0w35,0w38,275),
(0w42,0w42,275),
(0w48,0w58,275),
(0w64,0w90,275),
(0w97,0w122,275),
(0w34,0w34,276)], []), ([(0w32,0w33,275),
(0w35,0w38,275),
(0w42,0w42,275),
(0w48,0w58,275),
(0w64,0w90,275),
(0w97,0w122,275),
(0w34,0w34,276)], [33]), ([], [58])]
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
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_variables )
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_title )
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_commands )
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_Print )
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_SUM )
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_PROD )
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_TOSTRING )
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_endvars )
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_MEAN )
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_CORR )
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_MEDIAN )
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_STDEV )
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_VAR )
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_GETF )
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_GETI )
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_GETS )
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_TOFLOAT)
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_TOINT)
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_SUBS )
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_LINREG)
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_COV)
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_POW)
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_RT)
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;   T.VOID )
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_IF )
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_THEN )
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_ELSE )
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_WHILE )
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_DO )
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_END )
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.TIPO yytext 
      end
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.BOOL (valOf (Bool.fromString yytext)) 
      end
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.ID yytext 
      end
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.STR yytext
      end
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.NUM (valOf (Int.fromString yytext)) 
      end
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.REAL (valOf (Real.fromString yytext)) 
      end
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.SINT (Grammar.toIntList (Grammar.tokenize yytext))
      end
fun yyAction37 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         T.SFLOAT (Grammar.toFloatList (Grammar.tokenize yytext))
      end
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.SBOOL (Grammar.toBoolList (Grammar.tokenize yytext))
      end
fun yyAction39 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.SSTRING (Grammar.tokenize yytext)
      end
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;   T.EQ )
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;   T.EEQ )
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;   T.SEMI)
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;   T.PLUS )
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;  T.CONCAT)
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;   T.MINUS )
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;   T.TIMES )
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;   T.DIV )
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LP )
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;   T.RP )
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;   T.DOT )
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;   T.AND )
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;   T.OR )
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;   T.NOT )
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;   T.GEQ )
fun yyAction55 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LEQ )
fun yyAction56 (strm, lastMatch : yymatch) = (yystrm := strm;   T.GT )
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LT )
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm;   T.NEQ )
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm;   T.EMPTY )
fun yyAction60 (strm, lastMatch : yymatch) = (yystrm := strm;   T.COMMA )
fun yyAction61 (strm, lastMatch : yymatch) = (yystrm := strm;   T.DOTDOT )
fun yyAction62 (strm, lastMatch : yymatch) = (yystrm := strm;   continue() )
fun yyAction63 (strm, lastMatch : yymatch) = (yystrm := strm;
        T.KW_terminate )
fun yyAction64 (strm, lastMatch : yymatch) = let
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
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34,
  yyAction35, yyAction36, yyAction37, yyAction38, yyAction39, yyAction40,
  yyAction41, yyAction42, yyAction43, yyAction44, yyAction45, yyAction46,
  yyAction47, yyAction48, yyAction49, yyAction50, yyAction51, yyAction52,
  yyAction53, yyAction54, yyAction55, yyAction56, yyAction57, yyAction58,
  yyAction59, yyAction60, yyAction61, yyAction62, yyAction63, yyAction64])
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
