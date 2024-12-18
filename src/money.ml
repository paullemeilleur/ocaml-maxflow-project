

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
          | 'p' -> create_map graph line 

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment graph line
      in      
      loop graph2

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop empty_graph in
  
  close_in infile ;
  final_graph