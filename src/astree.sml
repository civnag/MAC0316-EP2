structure ParseTree = struct 

open Grammar

datatype Tree = Assign of (tipo AtomMap.map -> tipo AtomMap.map) * Tree 
              | Print of string * Tree
              | If of Tree * bool * Tree * Tree 
              | While of Tree * bool * (Tree list) 
              | Null


end