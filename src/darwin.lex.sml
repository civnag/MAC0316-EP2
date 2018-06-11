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
(0w58,0w58,1),
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
(0w59,0w59,15),
(0w60,0w60,16),
(0w61,0w61,17),
(0w62,0w62,18),
(0w65,0w90,19),
(0w97,0w97,19),
(0w104,0w104,19),
(0w106,0w107,19),
(0w110,0w111,19),
(0w113,0w114,19),
(0w117,0w117,19),
(0w120,0w122,19),
(0w98,0w98,20),
(0w99,0w99,21),
(0w100,0w100,22),
(0w101,0w101,23),
(0w102,0w102,24),
(0w103,0w103,25),
(0w105,0w105,26),
(0w108,0w108,27),
(0w109,0w109,28),
(0w112,0w112,29),
(0w115,0w115,30),
(0w116,0w116,31),
(0w118,0w118,32),
(0w119,0w119,33),
(0w123,0w123,34),
(0w124,0w124,35)], []), ([], [54]), ([], [52, 54]), ([(0w61,0w61,430)], [44, 54]), ([(0w65,0w90,428),
(0w97,0w122,428)], [54]), ([(0w38,0w38,427)], [54]), ([], [39, 54]), ([], [40, 54]), ([], [37, 54]), ([], [35, 54]), ([], [51, 54]), ([], [36, 54]), ([], [41, 54]), ([], [38, 54]), ([(0w34,0w34,421),
(0w46,0w46,421),
(0w48,0w57,422)], [19, 54]), ([], [34, 54]), ([(0w61,0w61,420)], [54]), ([(0w61,0w61,419)], [32, 54]), ([(0w61,0w61,418)], [54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,413)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,389)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,388)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w107,64),
(0w109,0w109,64),
(0w111,0w122,64),
(0w108,0w108,373),
(0w110,0w110,374)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w107,64),
(0w109,0w122,64),
(0w97,0w97,369),
(0w108,0w108,370)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,367)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w101,64),
(0w103,0w109,64),
(0w111,0w122,64),
(0w102,0w102,365),
(0w110,0w110,366)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,350)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,343)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,337)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w115,64),
(0w118,0w122,64),
(0w97,0w97,290),
(0w116,0w116,291),
(0w117,0w117,292)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w103,64),
(0w106,0w110,64),
(0w112,0w113,64),
(0w115,0w116,64),
(0w118,0w122,64),
(0w101,0w101,80),
(0w104,0w104,81),
(0w105,0w105,82),
(0w111,0w111,83),
(0w114,0w114,84),
(0w117,0w117,85)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w122,64),
(0w97,0w97,69)], [17, 54]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w103,64),
(0w105,0w122,64),
(0w104,0w104,65)], [17, 54]), ([(0w34,0w34,37),
(0w48,0w57,38),
(0w102,0w102,39),
(0w116,0w116,40),
(0w125,0w125,41)], [54]), ([(0w124,0w124,36)], [54]), ([], [43]), ([(0w65,0w90,60),
(0w97,0w122,60)], []), ([(0w34,0w34,49),
(0w46,0w46,49),
(0w44,0w44,50),
(0w48,0w57,38),
(0w125,0w125,51)], []), ([(0w97,0w97,47)], []), ([(0w114,0w114,42)], []), ([], [22, 23, 24, 25, 50]), ([(0w117,0w117,43)], []), ([(0w101,0w101,44)], []), ([(0w44,0w44,45),
(0w125,0w125,46)], []), ([(0w102,0w102,39),
(0w116,0w116,40)], []), ([], [24]), ([(0w108,0w108,48)], []), ([(0w115,0w115,43)], []), ([(0w48,0w57,53)], []), ([(0w48,0w57,52)], []), ([], [22]), ([(0w44,0w44,50),
(0w48,0w57,52),
(0w125,0w125,51)], []), ([(0w44,0w44,54),
(0w48,0w57,55),
(0w69,0w69,56),
(0w101,0w101,56),
(0w125,0w125,57)], []), ([(0w48,0w57,59)], []), ([(0w44,0w44,54),
(0w48,0w57,55),
(0w125,0w125,57)], []), ([(0w48,0w57,58)], []), ([], [23]), ([(0w44,0w44,54),
(0w48,0w57,58),
(0w125,0w125,57)], []), ([(0w34,0w34,49),
(0w46,0w46,49),
(0w48,0w57,59)], []), ([(0w34,0w34,61),
(0w48,0w57,60),
(0w65,0w90,60),
(0w97,0w122,60)], []), ([(0w44,0w44,62),
(0w125,0w125,63)], []), ([(0w34,0w34,37)], []), ([], [25]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,66)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,67)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,68)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [17, 29]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,70)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,71)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w122,64),
(0w97,0w97,72)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w97,64),
(0w99,0w109,64),
(0w111,0w122,64),
(0w98,0w98,73),
(0w110,0w110,74)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,77)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w98,64),
(0w100,0w122,64),
(0w99,0w99,75)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,76)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [12, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,78)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w114,64),
(0w116,0w122,64),
(0w115,0w115,79)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [0, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,283)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,281)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,278)], [17]), ([(0w48,0w57,64),
(0w65,0w82,64),
(0w84,0w90,64),
(0w97,0w122,64),
(0w83,0w83,272)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w116,64),
(0w118,0w122,64),
(0w117,0w117,270)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w111,64),
(0w113,0w122,64),
(0w112,0w112,86)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,87)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,88)], [17]), ([(0w40,0w40,89),
(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [17]), ([(0w98,0w98,90),
(0w102,0w102,91),
(0w105,0w105,92),
(0w115,0w115,93)], []), ([(0w111,0w111,265)], []), ([(0w108,0w108,263)], []), ([(0w110,0w110,262)], []), ([(0w116,0w116,94)], []), ([(0w114,0w114,95)], []), ([(0w105,0w105,96)], []), ([(0w110,0w110,97)], []), ([(0w103,0w103,98)], []), ([(0w44,0w44,99)], []), ([(0w98,0w98,100),
(0w102,0w102,101),
(0w105,0w105,102),
(0w115,0w115,103)], []), ([(0w111,0w111,257)], []), ([(0w108,0w108,255)], []), ([(0w110,0w110,254)], []), ([(0w116,0w116,104)], []), ([(0w114,0w114,105)], []), ([(0w105,0w105,106)], []), ([(0w110,0w110,107)], []), ([(0w103,0w103,108)], []), ([(0w41,0w41,109),
(0w44,0w44,110)], []), ([], [16]), ([(0w98,0w98,111),
(0w102,0w102,112),
(0w105,0w105,113),
(0w115,0w115,114)], []), ([(0w111,0w111,249)], []), ([(0w108,0w108,247)], []), ([(0w110,0w110,246)], []), ([(0w116,0w116,115)], []), ([(0w114,0w114,116)], []), ([(0w105,0w105,117)], []), ([(0w110,0w110,118)], []), ([(0w103,0w103,119)], []), ([(0w41,0w41,109),
(0w44,0w44,120)], []), ([(0w98,0w98,121),
(0w102,0w102,122),
(0w105,0w105,123),
(0w115,0w115,124)], []), ([(0w111,0w111,241)], []), ([(0w108,0w108,239)], []), ([(0w110,0w110,238)], []), ([(0w116,0w116,125)], []), ([(0w114,0w114,126)], []), ([(0w105,0w105,127)], []), ([(0w110,0w110,128)], []), ([(0w103,0w103,129)], []), ([(0w41,0w41,109),
(0w44,0w44,130)], []), ([(0w98,0w98,131),
(0w102,0w102,132),
(0w105,0w105,133),
(0w115,0w115,134)], []), ([(0w111,0w111,233)], []), ([(0w108,0w108,231)], []), ([(0w110,0w110,230)], []), ([(0w116,0w116,135)], []), ([(0w114,0w114,136)], []), ([(0w105,0w105,137)], []), ([(0w110,0w110,138)], []), ([(0w103,0w103,139)], []), ([(0w41,0w41,109),
(0w44,0w44,140)], []), ([(0w98,0w98,141),
(0w102,0w102,142),
(0w105,0w105,143),
(0w115,0w115,144)], []), ([(0w111,0w111,225)], []), ([(0w108,0w108,223)], []), ([(0w110,0w110,222)], []), ([(0w116,0w116,145)], []), ([(0w114,0w114,146)], []), ([(0w105,0w105,147)], []), ([(0w110,0w110,148)], []), ([(0w103,0w103,149)], []), ([(0w41,0w41,109),
(0w44,0w44,150)], []), ([(0w98,0w98,151),
(0w102,0w102,152),
(0w105,0w105,153),
(0w115,0w115,154)], []), ([(0w111,0w111,217)], []), ([(0w108,0w108,215)], []), ([(0w110,0w110,214)], []), ([(0w116,0w116,155)], []), ([(0w114,0w114,156)], []), ([(0w105,0w105,157)], []), ([(0w110,0w110,158)], []), ([(0w103,0w103,159)], []), ([(0w41,0w41,109),
(0w44,0w44,160)], []), ([(0w98,0w98,161),
(0w102,0w102,162),
(0w105,0w105,163),
(0w115,0w115,164)], []), ([(0w111,0w111,209)], []), ([(0w108,0w108,207)], []), ([(0w110,0w110,206)], []), ([(0w116,0w116,165)], []), ([(0w114,0w114,166)], []), ([(0w105,0w105,167)], []), ([(0w110,0w110,168)], []), ([(0w103,0w103,169)], []), ([(0w41,0w41,109),
(0w44,0w44,170)], []), ([(0w98,0w98,171),
(0w102,0w102,172),
(0w105,0w105,173),
(0w115,0w115,174)], []), ([(0w111,0w111,201)], []), ([(0w108,0w108,199)], []), ([(0w110,0w110,198)], []), ([(0w116,0w116,175)], []), ([(0w114,0w114,176)], []), ([(0w105,0w105,177)], []), ([(0w110,0w110,178)], []), ([(0w103,0w103,179)], []), ([(0w41,0w41,109),
(0w44,0w44,180)], []), ([(0w98,0w98,181),
(0w102,0w102,182),
(0w105,0w105,183),
(0w115,0w115,184)], []), ([(0w111,0w111,193)], []), ([(0w108,0w108,191)], []), ([(0w110,0w110,190)], []), ([(0w116,0w116,185)], []), ([(0w114,0w114,186)], []), ([(0w105,0w105,187)], []), ([(0w110,0w110,188)], []), ([(0w103,0w103,189)], []), ([(0w41,0w41,109)], []), ([(0w116,0w116,189)], []), ([(0w111,0w111,192)], []), ([(0w97,0w97,190)], []), ([(0w111,0w111,194)], []), ([(0w108,0w108,195)], []), ([(0w101,0w101,196)], []), ([(0w97,0w97,197)], []), ([(0w110,0w110,189)], []), ([(0w116,0w116,179)], []), ([(0w111,0w111,200)], []), ([(0w97,0w97,198)], []), ([(0w111,0w111,202)], []), ([(0w108,0w108,203)], []), ([(0w101,0w101,204)], []), ([(0w97,0w97,205)], []), ([(0w110,0w110,179)], []), ([(0w116,0w116,169)], []), ([(0w111,0w111,208)], []), ([(0w97,0w97,206)], []), ([(0w111,0w111,210)], []), ([(0w108,0w108,211)], []), ([(0w101,0w101,212)], []), ([(0w97,0w97,213)], []), ([(0w110,0w110,169)], []), ([(0w116,0w116,159)], []), ([(0w111,0w111,216)], []), ([(0w97,0w97,214)], []), ([(0w111,0w111,218)], []), ([(0w108,0w108,219)], []), ([(0w101,0w101,220)], []), ([(0w97,0w97,221)], []), ([(0w110,0w110,159)], []), ([(0w116,0w116,149)], []), ([(0w111,0w111,224)], []), ([(0w97,0w97,222)], []), ([(0w111,0w111,226)], []), ([(0w108,0w108,227)], []), ([(0w101,0w101,228)], []), ([(0w97,0w97,229)], []), ([(0w110,0w110,149)], []), ([(0w116,0w116,139)], []), ([(0w111,0w111,232)], []), ([(0w97,0w97,230)], []), ([(0w111,0w111,234)], []), ([(0w108,0w108,235)], []), ([(0w101,0w101,236)], []), ([(0w97,0w97,237)], []), ([(0w110,0w110,139)], []), ([(0w116,0w116,129)], []), ([(0w111,0w111,240)], []), ([(0w97,0w97,238)], []), ([(0w111,0w111,242)], []), ([(0w108,0w108,243)], []), ([(0w101,0w101,244)], []), ([(0w97,0w97,245)], []), ([(0w110,0w110,129)], []), ([(0w116,0w116,119)], []), ([(0w111,0w111,248)], []), ([(0w97,0w97,246)], []), ([(0w111,0w111,250)], []), ([(0w108,0w108,251)], []), ([(0w101,0w101,252)], []), ([(0w97,0w97,253)], []), ([(0w110,0w110,119)], []), ([(0w116,0w116,108)], []), ([(0w111,0w111,256)], []), ([(0w97,0w97,254)], []), ([(0w111,0w111,258)], []), ([(0w108,0w108,259)], []), ([(0w101,0w101,260)], []), ([(0w97,0w97,261)], []), ([(0w110,0w110,108)], []), ([(0w116,0w116,98)], []), ([(0w111,0w111,264)], []), ([(0w97,0w97,262)], []), ([(0w111,0w111,266)], []), ([(0w108,0w108,267)], []), ([(0w101,0w101,268)], []), ([(0w97,0w97,269)], []), ([(0w110,0w110,98)], []), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,271)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [17, 21]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,273)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,274)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,275)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,276)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w102,64),
(0w104,0w122,64),
(0w103,0w103,277)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [6, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,279)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,280)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [1, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,282)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [17, 27]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w108,64),
(0w110,0w122,64),
(0w109,0w109,284)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,285)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,286)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w122,64),
(0w97,0w97,287)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,288)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,289)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [17, 53]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w108,64),
(0w110,0w122,64),
(0w109,0w109,308)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w99,64),
(0w101,0w113,64),
(0w115,0w122,64),
(0w100,0w100,294),
(0w114,0w114,295)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w108,64),
(0w110,0w122,64),
(0w109,0w109,293)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [4, 17]), ([(0w48,0w57,64),
(0w65,0w67,64),
(0w69,0w90,64),
(0w97,0w122,64),
(0w68,0w68,299)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,296)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,297)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w102,64),
(0w104,0w122,64),
(0w103,0w103,298)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [16, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,300)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w117,64),
(0w119,0w122,64),
(0w118,0w118,301)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,302)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w122,64),
(0w97,0w97,303)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,304)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,305)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,306)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,307)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [11, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w111,64),
(0w113,0w122,64),
(0w112,0w112,309)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,310)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,311)], [17]), ([(0w32,0w32,312),
(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [17]), ([(0w111,0w111,313)], []), ([(0w102,0w102,314)], []), ([(0w32,0w32,315)], []), ([(0w98,0w98,316),
(0w102,0w102,317),
(0w105,0w105,318),
(0w115,0w115,319),
(0w116,0w116,320)], []), ([(0w111,0w111,332)], []), ([(0w108,0w108,330)], []), ([(0w110,0w110,329)], []), ([(0w116,0w116,325)], []), ([(0w117,0w117,321)], []), ([(0w112,0w112,322)], []), ([(0w108,0w108,323)], []), ([(0w101,0w101,324)], []), ([(0w40,0w40,89)], []), ([(0w114,0w114,326)], []), ([(0w105,0w105,327)], []), ([(0w110,0w110,328)], []), ([(0w103,0w103,109)], []), ([(0w116,0w116,109)], []), ([(0w111,0w111,331)], []), ([(0w97,0w97,329)], []), ([(0w111,0w111,333)], []), ([(0w108,0w108,334)], []), ([(0w101,0w101,335)], []), ([(0w97,0w97,336)], []), ([(0w110,0w110,109)], []), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w104,64),
(0w106,0w110,64),
(0w112,0w122,64),
(0w105,0w105,338),
(0w111,0w111,339)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,341)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w99,64),
(0w101,0w122,64),
(0w100,0w100,340)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [5, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,342)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [3, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w99,64),
(0w101,0w122,64),
(0w97,0w97,344),
(0w100,0w100,345)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,349)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,346)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w122,64),
(0w97,0w97,347)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,348)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [10, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [8, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,351)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,352)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w122,64),
(0w97,0w97,353)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,354)], [17]), ([(0w48,0w57,64),
(0w65,0w81,64),
(0w83,0w90,64),
(0w97,0w122,64),
(0w82,0w82,355)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,356)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w102,64),
(0w104,0w122,64),
(0w103,0w103,357)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,358)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,359)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w114,64),
(0w116,0w122,64),
(0w115,0w115,360)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w114,64),
(0w116,0w122,64),
(0w115,0w115,361)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,362)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,363)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,364)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [14, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [17, 26]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,298)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,368)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [13, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,372)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,371)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w122,64),
(0w97,0w97,366)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w114,64),
(0w116,0w122,64),
(0w115,0w115,270)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w114,64),
(0w116,0w122,64),
(0w115,0w115,386)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w99,64),
(0w101,0w122,64),
(0w100,0w100,375)], [17]), ([(0w32,0w32,376),
(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [17, 31]), ([(0w118,0w118,377)], []), ([(0w97,0w97,378)], []), ([(0w114,0w114,379)], []), ([(0w105,0w105,380)], []), ([(0w97,0w97,381)], []), ([(0w98,0w98,382)], []), ([(0w108,0w108,383)], []), ([(0w101,0w101,384)], []), ([(0w115,0w115,385)], []), ([], [7]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,387)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [17, 28]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [17, 30]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w108,64),
(0w110,0w113,64),
(0w115,0w117,64),
(0w119,0w122,64),
(0w109,0w109,390),
(0w114,0w114,391),
(0w118,0w118,392)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w108,64),
(0w110,0w122,64),
(0w109,0w109,408)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,400)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w122,64),
(0w97,0w97,393)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w113,64),
(0w115,0w122,64),
(0w114,0w114,394)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,395)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w122,64),
(0w97,0w97,396)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,397)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w98,64),
(0w100,0w122,64),
(0w99,0w99,398)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,399)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [15, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,401)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,402)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w122,64),
(0w97,0w97,403)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w115,64),
(0w117,0w122,64),
(0w116,0w116,404)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w104,64),
(0w106,0w122,64),
(0w105,0w105,405)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,406)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,407)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [9, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w122,64),
(0w97,0w97,409)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,410)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w99,64),
(0w101,0w122,64),
(0w100,0w100,411)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w114,64),
(0w116,0w122,64),
(0w115,0w115,412)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w122,64)], [2, 17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w110,64),
(0w112,0w122,64),
(0w111,0w111,414)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w107,64),
(0w109,0w122,64),
(0w108,0w108,415)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w100,64),
(0w102,0w122,64),
(0w101,0w101,416)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w98,0w122,64),
(0w97,0w97,417)], [17]), ([(0w48,0w57,64),
(0w65,0w90,64),
(0w97,0w109,64),
(0w111,0w122,64),
(0w110,0w110,298)], [17]), ([], [45, 47]), ([], [33]), ([], [46, 48]), ([(0w48,0w57,423)], []), ([(0w34,0w34,421),
(0w46,0w46,421),
(0w48,0w57,422)], [19]), ([(0w48,0w57,424),
(0w69,0w69,425),
(0w101,0w101,425)], [20]), ([(0w48,0w57,424)], [20]), ([(0w48,0w57,426)], []), ([(0w48,0w57,426)], [20]), ([], [42]), ([(0w34,0w34,429),
(0w48,0w57,428),
(0w65,0w90,428),
(0w97,0w122,428)], []), ([], [18]), ([], [49])]
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
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_GET )
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_LINREG)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_COV)
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.TIPO yytext 
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.ID yytext 
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.STR yytext
      end
fun yyAction19 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.NUM (valOf (Int.fromString yytext)) 
      end
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.REAL (valOf (Real.fromString yytext)) 
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.BOOL (valOf (Bool.fromString yytext)) 
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.SINT (Grammar.toIntList (Grammar.tokenize yytext))
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         T.SFLOAT (Grammar.toFloatList (Grammar.tokenize yytext))
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.SBOOL (Grammar.toBoolList (Grammar.tokenize yytext))
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.SSTRING (Grammar.tokenize yytext)
      end
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_IF )
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_THEN )
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_ELSE )
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_WHILE )
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_DO )
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;   T.KW_END )
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;   T.EQ )
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;   T.EEQ )
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;   T.SEMI)
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;   T.PLUS )
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;   T.MINUS )
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;   T.TIMES )
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;   T.DIV )
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LP )
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;   T.RP )
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;   T.DOT )
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;   T.AND )
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;   T.OR )
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;   T.NOT )
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;   T.GEQ )
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LEQ )
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;   T.GT )
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LT )
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;   T.NEQ )
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;   T.EMPTY )
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;   T.COMMA )
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;   continue() )
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
        T.KW_terminate )
fun yyAction54 (strm, lastMatch : yymatch) = let
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
  yyAction53, yyAction54])
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
