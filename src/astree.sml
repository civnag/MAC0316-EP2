structure ParseTree = struct 

open Grammar

datatype Tree = Assign of (tipo AtomMap.map -> tipo AtomMap.map) 
              | Print of string 
              | If of bool * (Tree list) * (Tree list) 
              | While of bool * (Tree list) 
              | Null

type RoseTree = Tree list

fun interpret(Print(s)::cs, am: tipo AtomMap.map): unit = 
    let 
        val _ = (print (s ^ "\n")) 
    in 
        interpret(cs,am)
    end
  | interpret(Assign(f)::cs, am: tipo AtomMap.map) = 
    let 
        val _ = (f am)
        val _ = "assign\n"
    in
        interpret(cs,am)
    end
  | interpret(If(b,l,r)::cs,am: tipo AtomMap.map) =
        let 
            val _ = if b then interpret(l,am) else interpret(r,am)
        in
            interpret(cs,am)
        end
  | interpret(Null::cs,am: tipo AtomMap.map) = ()
  | interpret(nil,am: tipo AtomMap.map) = ()
end