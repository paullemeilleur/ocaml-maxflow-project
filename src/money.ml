open Graph

let create_liste liste line  =
  try Scanf.sscanf line "p %s %d" (fun personne somme -> (personne, somme) :: liste)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"


let rec add_id liste compteur (map: (int, 'a *'b) Hashtbl.t)  =
  match liste with
  | [] -> map
  | (personne, somme) :: rest -> (Hashtbl.add map compteur (personne,somme)); add_id rest (compteur + 1) map;;


let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

    
let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 'p' -> create_liste graph line 

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment graph line
      in      
      loop graph2

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop [] in
  
  close_in infile ;
  final_graph

let create_hashtable liste =
  let map = Hashtbl.create (List.length liste) in
  add_id liste 0 map;;

let create_all_nodes graphe (hastable: (int, 'a *'b) Hashtbl.t) =
  let graph = ref graphe in
  Hashtbl.iter (fun id _ -> graph := new_node !graph id) hastable;
  !graph

let add_source_node graph = (* Ajoute le noeud source, pour les personnes en négatif *)
  new_node graph (-1)

let add_destination_node graph = (* Ajoute le noeud destination, pour les personnes en positif *)
  new_node graph (-2)

(* Fonction qui ajoute les arcs vers la source et la destination *)
let add_arcs graph hastable moyenne = 
  let graph = ref graph in
  Hashtbl.iter (fun id (_, somme) -> 
    if somme > moyenne then
      graph := new_arc !graph {src = id; tgt = (-2); lbl = (somme - moyenne)}
    else
      graph := new_arc !graph {src = (-1); tgt = id; lbl = (moyenne - somme)}
  ) hastable;
  !graph

let add_arcs_between_nodes graph hastable = 
  let graph = ref graph in
  Hashtbl.iter (fun id (_, _) -> 
    Hashtbl.iter (fun id2 (_, _) -> 
      if id <> id2 then
        graph := new_arc !graph {src = id; tgt = id2; lbl = 0}
    ) hastable
  ) hastable;
  !graph

let calculate_moyenne hastable =
  let somme = ref 0 in
  let nb_personnes = ref 0 in
  Hashtbl.iter (fun _ (_, s) -> somme := !somme + s; nb_personnes := !nb_personnes + 1) hastable;
  !somme / !nb_personnes


