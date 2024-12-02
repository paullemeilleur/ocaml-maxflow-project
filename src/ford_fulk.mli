open Graph


type flot =
  { 
    (* Capacité *)
    capa: int ;

    (* Flot *)  
    flot: int}

(* Création d'un graphe flot *)
val create_flot_graph: int graph -> flot graph;;

(* Ecart *)
val arcs_ecart: flot arc -> (int arc * int arc);;

(*Graph ecart*)
val graph_ecart: flot graph -> int graph;;

(* Ford-Fulkerson 
val ford_fulkerson: 'a graph -> id -> id -> 'a graph;;*)