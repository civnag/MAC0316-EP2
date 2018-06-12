structure ParseTree = struct 

open Grammar

datatype Tree = Assign of (tipo AtomMap.map -> tipo AtomMap.map) * Tree 
              | Print of string * Tree
              | If of Tree * bool * Tree * Tree 
              | While of Tree * bool * (Tree list) 
              | Null

fun interpret(Print(s,t), am) = 
    let 
        val _ = (print s)
    in 
        interpret(t,am)
    end
fun interpret(Assign(f,t), am) = 
    let 
        val _ = (f am)
    in
        interpret(t,am)
    end
fun interpret(Null,am): unit = ()
fun interpret(If(_,b,l,r),am) =
    if b then
        interpret(l,am)
    else
        interpret(r,am)
end