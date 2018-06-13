structure DarwinLexer  = struct

    datatype yystart_state = 
INITIAL
    local

    structure UserDeclarations = 
      struct

 
    structure T = DarwinTokens
    type lex_result = T.token
    fun eof() = T.EOF

      end

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
(0w124,0w124,37)], []), ([], [64]), ([], [62, 64]), ([(0w61,0w61,282)], [53, 64]), ([(0w32,0w33,280),
(0w35,0w38,280),
(0w42,0w42,280),
(0w48,0w58,280),
(0w64,0w90,280),
(0w97,0w122,280),
(0w34,0w34,281)], [64]), ([(0w38,0w38,279)], [64]), ([], [48, 64]), ([], [49, 64]), ([], [46, 64]), ([(0w43,0w43,278),
(0w48,0w57,273)], [43, 64]), ([], [60, 64]), ([(0w48,0w57,273)], [45, 64]), ([], [50, 64]), ([], [47, 64]), ([(0w34,0w34,272),
(0w46,0w46,272),
(0w48,0w57,273)], [34, 64]), ([(0w61,0w61,271)], [64]), ([], [42, 64]), ([(0w61,0w61,270)], [57, 64]), ([(0w61,0w61,269)], [40, 64]), ([(0w61,0w61,268)], [56, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w110,68),
(0w112,0w122,68),
(0w111,0w111,263)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w110,68),
(0w112,0w122,68),
(0w111,0w111,239)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w110,68),
(0w112,0w122,68),
(0w111,0w111,238)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w107,68),
(0w109,0w109,68),
(0w111,0w122,68),
(0w108,0w108,223),
(0w110,0w110,224)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w107,68),
(0w109,0w122,68),
(0w97,0w97,219),
(0w108,0w108,220)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,203)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w101,68),
(0w103,0w109,68),
(0w111,0w122,68),
(0w102,0w102,201),
(0w110,0w110,202)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,186)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,179)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w110,68),
(0w112,0w113,68),
(0w115,0w122,68),
(0w111,0w111,171),
(0w114,0w114,172)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,170)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w115,68),
(0w118,0w122,68),
(0w97,0w97,120),
(0w116,0w116,121),
(0w117,0w117,122)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w103,68),
(0w106,0w110,68),
(0w112,0w113,68),
(0w115,0w122,68),
(0w101,0w101,87),
(0w104,0w104,88),
(0w105,0w105,89),
(0w111,0w111,90),
(0w114,0w114,91)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w110,68),
(0w112,0w122,68),
(0w97,0w97,73),
(0w111,0w111,74)], [32, 64]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w103,68),
(0w105,0w122,68),
(0w104,0w104,69)], [32, 64]), ([(0w34,0w34,39),
(0w43,0w43,40),
(0w45,0w45,40),
(0w48,0w57,41),
(0w102,0w102,42),
(0w116,0w116,43),
(0w125,0w125,44)], [64]), ([(0w124,0w124,38)], [64]), ([], [52]), ([(0w32,0w33,39),
(0w35,0w38,39),
(0w42,0w42,39),
(0w48,0w58,39),
(0w64,0w90,39),
(0w97,0w122,39),
(0w34,0w34,65)], []), ([(0w48,0w57,41)], []), ([(0w34,0w34,52),
(0w46,0w46,52),
(0w44,0w44,53),
(0w48,0w57,41),
(0w125,0w125,54)], []), ([(0w97,0w97,50)], []), ([(0w114,0w114,45)], []), ([], [36, 37, 38, 39, 59]), ([(0w117,0w117,46)], []), ([(0w101,0w101,47)], []), ([(0w44,0w44,48),
(0w125,0w125,49)], []), ([(0w102,0w102,42),
(0w116,0w116,43)], []), ([], [38]), ([(0w108,0w108,51)], []), ([(0w115,0w115,46)], []), ([(0w48,0w57,57)], []), ([(0w43,0w43,55),
(0w45,0w45,55),
(0w48,0w57,56)], []), ([], [36]), ([(0w48,0w57,56)], []), ([(0w44,0w44,53),
(0w48,0w57,56),
(0w125,0w125,54)], []), ([(0w44,0w44,58),
(0w48,0w57,57),
(0w69,0w69,59),
(0w101,0w101,59),
(0w125,0w125,60)], []), ([(0w43,0w43,63),
(0w45,0w45,63),
(0w48,0w57,64)], []), ([(0w43,0w43,61),
(0w45,0w45,61),
(0w48,0w57,62)], []), ([], [37]), ([(0w48,0w57,62)], []), ([(0w44,0w44,58),
(0w48,0w57,62),
(0w125,0w125,60)], []), ([(0w48,0w57,64)], []), ([(0w34,0w34,52),
(0w46,0w46,52),
(0w48,0w57,64)], []), ([(0w32,0w33,39),
(0w35,0w38,39),
(0w42,0w42,39),
(0w48,0w58,39),
(0w64,0w90,39),
(0w97,0w122,39),
(0w34,0w34,65),
(0w44,0w44,66),
(0w125,0w125,67)], []), ([(0w34,0w34,39)], []), ([], [39]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,70)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w107,68),
(0w109,0w122,68),
(0w108,0w108,71)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,72)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [27, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w113,68),
(0w115,0w122,68),
(0w114,0w114,77)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,75)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w99,68),
(0w101,0w122,68),
(0w100,0w100,76)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [23, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,78)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,79)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w97,68),
(0w99,0w109,68),
(0w111,0w122,68),
(0w98,0w98,80),
(0w110,0w110,81)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w107,68),
(0w109,0w122,68),
(0w108,0w108,84)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w98,68),
(0w100,0w122,68),
(0w99,0w99,82)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,83)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [12, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,85)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w114,68),
(0w116,0w122,68),
(0w115,0w115,86)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [0, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w113,68),
(0w115,0w122,68),
(0w114,0w114,113)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,111)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,108)], [32]), ([(0w48,0w57,68),
(0w65,0w69,68),
(0w71,0w72,68),
(0w74,0w82,68),
(0w84,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68),
(0w70,0w70,94),
(0w73,0w73,95),
(0w83,0w83,96)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w116,68),
(0w118,0w122,68),
(0w117,0w117,92)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,93)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [31, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w107,68),
(0w109,0w122,68),
(0w108,0w108,104)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,102)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,97)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w113,68),
(0w115,0w122,68),
(0w114,0w114,98)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,99)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,100)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w102,68),
(0w104,0w122,68),
(0w103,0w103,101)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [6, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,103)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [17, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w110,68),
(0w112,0w122,68),
(0w111,0w111,105)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,106)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,107)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [16, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w107,68),
(0w109,0w122,68),
(0w108,0w108,109)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,110)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [1, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,112)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [25, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w108,68),
(0w110,0w122,68),
(0w109,0w109,114)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,115)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,116)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,117)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,118)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,119)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [32, 63]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w108,68),
(0w110,0w122,68),
(0w109,0w109,145)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w99,68),
(0w101,0w113,68),
(0w115,0w122,68),
(0w100,0w100,131),
(0w114,0w114,132)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w97,68),
(0w99,0w108,68),
(0w110,0w122,68),
(0w98,0w98,123),
(0w109,0w109,124)], [32]), ([(0w48,0w57,68),
(0w65,0w82,68),
(0w84,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68),
(0w83,0w83,125)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [4, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,126)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w108,68),
(0w110,0w122,68),
(0w109,0w109,127)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w111,68),
(0w113,0w122,68),
(0w112,0w112,128)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w107,68),
(0w109,0w122,68),
(0w108,0w108,129)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,130)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [18, 32]), ([(0w48,0w57,68),
(0w65,0w67,68),
(0w69,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68),
(0w68,0w68,136)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,133)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,134)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w102,68),
(0w104,0w122,68),
(0w103,0w103,135)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [30, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,137)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w117,68),
(0w119,0w122,68),
(0w118,0w118,138)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,139)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,140)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,141)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,142)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w110,68),
(0w112,0w122,68),
(0w111,0w111,143)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,144)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [11, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w111,68),
(0w113,0w122,68),
(0w112,0w112,146)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w107,68),
(0w109,0w122,68),
(0w108,0w108,147)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,148)], [32]), ([(0w32,0w32,149),
(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [32]), ([(0w111,0w111,150)], []), ([(0w102,0w102,151)], []), ([(0w32,0w32,152)], []), ([(0w98,0w98,153),
(0w102,0w102,154),
(0w105,0w105,155),
(0w115,0w115,156)], []), ([(0w111,0w111,165)], []), ([(0w108,0w108,163)], []), ([(0w110,0w110,162)], []), ([(0w116,0w116,157)], []), ([(0w114,0w114,158)], []), ([(0w105,0w105,159)], []), ([(0w110,0w110,160)], []), ([(0w103,0w103,161)], []), ([], [30]), ([(0w116,0w116,161)], []), ([(0w111,0w111,164)], []), ([(0w97,0w97,162)], []), ([(0w111,0w111,166)], []), ([(0w108,0w108,167)], []), ([(0w101,0w101,168)], []), ([(0w97,0w97,169)], []), ([(0w110,0w110,161)], []), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [22, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w118,68),
(0w120,0w122,68),
(0w119,0w119,178)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w110,68),
(0w112,0w122,68),
(0w105,0w105,173),
(0w111,0w111,174)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,176)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w99,68),
(0w101,0w122,68),
(0w100,0w100,175)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [5, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,177)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [3, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [21, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w99,68),
(0w101,0w122,68),
(0w97,0w97,180),
(0w100,0w100,181)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,185)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,182)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,183)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,184)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [10, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [8, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,187)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,188)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,189)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w113,68),
(0w115,0w122,68),
(0w114,0w114,190)], [32]), ([(0w48,0w57,68),
(0w65,0w81,68),
(0w83,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68),
(0w82,0w82,191)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,192)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w102,68),
(0w104,0w122,68),
(0w103,0w103,193)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w113,68),
(0w115,0w122,68),
(0w114,0w114,194)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,195)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w114,68),
(0w116,0w122,68),
(0w115,0w115,196)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w114,68),
(0w116,0w122,68),
(0w115,0w115,197)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,198)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w110,68),
(0w112,0w122,68),
(0w111,0w111,199)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,200)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [19, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [24, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,135)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,204)], [32]), ([(0w48,0w57,68),
(0w65,0w69,68),
(0w71,0w72,68),
(0w74,0w82,68),
(0w84,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68),
(0w70,0w70,205),
(0w73,0w73,206),
(0w83,0w83,207)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w107,68),
(0w109,0w122,68),
(0w108,0w108,215)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,213)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,208)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w113,68),
(0w115,0w122,68),
(0w114,0w114,209)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,210)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,211)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w102,68),
(0w104,0w122,68),
(0w103,0w103,212)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [15, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,214)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [14, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w110,68),
(0w112,0w122,68),
(0w111,0w111,216)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,217)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,218)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [13, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w107,68),
(0w109,0w122,68),
(0w108,0w108,222)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w110,68),
(0w112,0w122,68),
(0w111,0w111,221)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,202)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w114,68),
(0w116,0w122,68),
(0w115,0w115,92)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w114,68),
(0w116,0w122,68),
(0w115,0w115,236)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w99,68),
(0w101,0w122,68),
(0w100,0w100,225)], [32]), ([(0w32,0w32,226),
(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [29, 32]), ([(0w118,0w118,227)], []), ([(0w97,0w97,228)], []), ([(0w114,0w114,229)], []), ([(0w105,0w105,230)], []), ([(0w97,0w97,231)], []), ([(0w98,0w98,232)], []), ([(0w108,0w108,233)], []), ([(0w101,0w101,234)], []), ([(0w115,0w115,235)], []), ([], [7]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,237)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [26, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [28, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w108,68),
(0w110,0w113,68),
(0w115,0w117,68),
(0w119,0w122,68),
(0w109,0w109,240),
(0w114,0w114,241),
(0w118,0w118,242)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w108,68),
(0w110,0w122,68),
(0w109,0w109,258)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w113,68),
(0w115,0w122,68),
(0w114,0w114,250)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,243)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w113,68),
(0w115,0w122,68),
(0w114,0w114,244)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,245)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,246)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,247)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w98,68),
(0w100,0w122,68),
(0w99,0w99,248)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,249)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [20, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,251)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w107,68),
(0w109,0w122,68),
(0w108,0w108,252)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,253)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w115,68),
(0w117,0w122,68),
(0w116,0w116,254)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w104,68),
(0w106,0w122,68),
(0w105,0w105,255)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w110,68),
(0w112,0w122,68),
(0w111,0w111,256)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,257)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [9, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,259)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,260)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w99,68),
(0w101,0w122,68),
(0w100,0w100,261)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w114,68),
(0w116,0w122,68),
(0w115,0w115,262)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w122,68)], [2, 32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w110,68),
(0w112,0w122,68),
(0w111,0w111,264)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w107,68),
(0w109,0w122,68),
(0w108,0w108,265)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w100,68),
(0w102,0w122,68),
(0w101,0w101,266)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w98,0w122,68),
(0w97,0w97,267)], [32]), ([(0w48,0w57,68),
(0w65,0w90,68),
(0w95,0w95,68),
(0w97,0w109,68),
(0w111,0w122,68),
(0w110,0w110,135)], [32]), ([], [54]), ([], [41]), ([], [55]), ([], [61]), ([(0w48,0w57,274)], []), ([(0w34,0w34,272),
(0w46,0w46,272),
(0w48,0w57,273)], [34]), ([(0w48,0w57,274),
(0w69,0w69,275),
(0w101,0w101,275)], [35]), ([(0w43,0w43,276),
(0w45,0w45,276),
(0w48,0w57,277)], []), ([(0w48,0w57,277)], []), ([(0w48,0w57,277)], [35]), ([], [44]), ([], [51]), ([(0w32,0w33,280),
(0w35,0w38,280),
(0w42,0w42,280),
(0w48,0w58,280),
(0w64,0w90,280),
(0w97,0w122,280),
(0w34,0w34,281)], []), ([(0w32,0w33,280),
(0w35,0w38,280),
(0w42,0w42,280),
(0w48,0w58,280),
(0w64,0w90,280),
(0w97,0w122,280),
(0w34,0w34,281)], [33]), ([], [58])]
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
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
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
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_comands )
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
