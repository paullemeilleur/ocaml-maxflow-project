open Graph
open Gfile


(* Ford-Fulkerson *)
val ford_fulkerson: id graph -> id -> id -> (path graph -> path -> unit)-> unit;;