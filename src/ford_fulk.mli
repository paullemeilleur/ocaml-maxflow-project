open Graph


type flot =
  { 
    (* Capacité *)
    capa: int ;

    (* Flot *)  
    flot: int}

(* Création d'un graphe flot *)
val create_flot_graph: int graph -> flot graph;;

(*Graph ecart*)
val graph_ecart: flot graph -> int graph;;

(* Find path *)
val find_path: int graph -> id -> id -> id list;;

(* Find minimum value of the path*)
val min_path_value: int graph -> id list -> int;;

(* Update flot graph *)
val update_path: int graph -> id list -> int -> int graph;;

(* Ford-Fulkerson 
val ford_fulkerson: 'a graph -> id -> id -> 'a graph;;*)