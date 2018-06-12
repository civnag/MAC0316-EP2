structure ParseTree = struct 

open Grammar

datatype Tree = Assign of (tipo AtomMap.map -> unit) * Tree
              | Print of string * Tree
              | If of bool * Tree * Tree 
              | While of bool * (Tree list) * Tree
              | Null



end