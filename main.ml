open Unix
open Str


(* Récupération de la string dans le fichier *)

  let read_txt name =
  let channel = open_in name in
  let contents = input_line channel in
  close_in channel;
  contents


(* Process du string vers un liste de liste de string sous forme quasi-cnf  *)


let replace_operators str =
  let replaced_and = Str.global_replace (Str.regexp "and") "&" str in
  Str.global_replace (Str.regexp "or") "|" replaced_and 

let remove_spaces s =
  String.concat "" (String.split_on_char ' ' s)
  
    
let split_by_and_or s =
  s |> String.split_on_char '&' |> List.map (fun x -> String.split_on_char '|' x)



let rec remove_useless_parenthesis l = 

  let aux t = 
    match t with 
    |t when List.length t > 1 -> 
            let h,q = List.hd t , List.tl t in 
            let q' = List.rev q in 
            let h' = List.hd q' in
            let q'' = List.tl q' in
            let h_m = String.sub h 1 ( String.length h -1 ) in
            let h'_m = String.sub h' 0 ( String.length h' -1 ) in
            h_m :: ( List.rev ( h'_m :: q'' ) ) 
    | _ -> t 
  in 
  match l with 
    |[] -> [] 
    | t :: q -> (aux t) :: remove_useless_parenthesis q


let process s =
  s |> replace_operators |> remove_spaces |> split_by_and_or |> remove_useless_parenthesis




(* creation de la table de hachage et des variables *)


let hash form = 
  let i = ref 0 in 
  let ht = Hashtbl.create 10 in

  let rec create l l' = 
    match l with 
    | [] -> l'
    | t :: q -> 
         i := !i + 1 ;
          Hashtbl.add ht (string_of_int (!i) ) t ; 
          create q ( l' @ [(string_of_int (!i) )] );
  in 
  
  let rec aux l = 
  match l with 
    | [] -> [] 
    | t :: q -> (create t []) :: aux q 
    in 
    let f' = aux form in 
    
   ht,f'


(* écriture de la cnf dans un fichier *)

let dimacs_cnf_of_string_lists string_lists =
  let num_vars = List.length (List.concat string_lists) in
  let clauses = List.map (fun clause -> clause @ ["0"]) string_lists in
  let num_clauses = List.length clauses in
  let header = Printf.sprintf "c \np cnf %d %d\n" num_vars num_clauses in
  let body = String.concat "\n" (List.map (fun clause -> String.concat " " clause) clauses) in
  header ^ body


let write name s =
  let channel = open_out name in
  output_string channel s;
  close_out channel

let output name f =
  let cnf = dimacs_cnf_of_string_lists f in
  write name cnf



(* appel glucose *)


let run_glucose cnf_file =
  let cmd = Printf.sprintf "./glucose %s -model" cnf_file in
  let ic = Unix.open_process_in cmd in
  let rec read_lines acc =
    match input_line ic with
    | exception End_of_file -> acc
    | line -> read_lines (line :: acc)
  in
  let lines = read_lines [] in
  let status = (List.hd lines),( List.hd ( List.tl lines) ) in
 
  match status with
  |s when (fst(status)).[0] = 'v' ->
      let model = fst(status) in
      let sat = snd(status) in
      (sat, model)
  | _ -> (fst(status), "")


(* appel glpk *)



(* modification cnf *)



(* main *)


let main () =
  let args = Sys.argv in
  if Array.length args < 3 then
    Printf.printf "Usage: %s <input_file> <output_file>\n" args.(0)
  else

    let input_file = args.(1) in
    let output_file = args.(2) in

    try 
      let s = read_txt input_file in 
      let s' = process s in
      let ht,f = hash s' in
      output output_file f ;
      Printf.printf "Successfully wrote formula in %s\n" output_file ;
      let (status, model) = run_glucose output_file in
      Printf.printf "%s\n%s\n" status model
    with
    | Sys_error msg -> Printf.printf "Error: %s\n" msg
    | exn -> Printf.printf "Error: %s\n" (Printexc.to_string exn)

let () = main ()