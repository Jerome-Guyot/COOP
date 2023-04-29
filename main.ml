open Unix
open Str


(* Récupération de la string dans le fichier *)

  let read_txt name =
  let channel = open_in name in
  let contents = input_line channel in
  close_in channel;
  contents


(* Process du string vers un liste de liste de string sous forme quasi-cnf  *)

(* " ( ( a < 7 ) or ( b > 6 ) or ( d > 7) ) and ( c > 7 ) " deviens [["(a<7)"; "(b>6)"; "(d>7)"]; ["(c>7)"]] *)


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
  s |> replace_operators |> remove_spaces |> split_by_and_or (* |> remove_useless_parenthesis *) (* car ici on enlève toutes les parenthèses dans la ht *)



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

let erase_parenthesis ht =
  Hashtbl.iter (fun key value ->
    Hashtbl.replace ht key (String.map (function
      | '(' -> ' '
      | ')' -> ' '
      | c -> c) value)) ht


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



(* modification cnf *)

let find_op clause =
  let rec aux_find_op i =
    if i >= String.length clause then failwith "no op found"
    else if clause.[i] = '!' then "!=", i
    else if clause.[i] = '=' then "=", i
    else if clause.[i] = '<' then 
      if i+1 < String.length clause && clause.[i+1] = '='
      then "<=", i
      else "<", i
    else if clause.[i] = '>' then 
      if i+1 < String.length clause && clause.[i+1] = '='
      then ">=", i
      else ">", i
    else aux_find_op (i+1)
  in aux_find_op 0

let negate_clause clause =
  let op, ind = find_op clause in
  match op with
  | "=" -> String.sub clause 0 ind ^ "!=" ^ String.sub clause (ind+1) (String.length clause - ind - 1)
  | "<" -> String.sub clause 0 ind ^ ">=" ^ String.sub clause (ind+1) (String.length clause - ind - 1)
  | ">" -> String.sub clause 0 ind ^ "<=" ^ String.sub clause (ind+1) (String.length clause - ind - 1)
  | "<=" -> String.sub clause 0 ind ^ ">" ^ String.sub clause (ind+2) (String.length clause - ind - 2)
  | ">=" -> String.sub clause 0 ind ^ "<" ^ String.sub clause (ind+2) (String.length clause - ind - 2)
  | "!=" -> String.sub clause 0 ind ^ "=" ^ String.sub clause (ind+2) (String.length clause - ind - 2)
  | _ -> failwith "op not supported"

let is_neq clause =
  let op, ind = find_op clause in 
  match op with
  | s when s = "!=" -> 
    [
      String.sub clause 0 ind ^ "<" ^ String.sub clause (ind+2) (String.length clause - ind - 2);
      String.sub clause 0 ind ^ ">" ^ String.sub clause (ind+2) (String.length clause - ind - 2);
    ]
  | _ -> [clause]



(* appel glpk *)

let initialize_problem model clauses_table =
  let problem = ref "maximize\n\t0\nsubject to\n" in
  let ind_to_clauses i =
    if i > 0 then
      is_neq (Hashtbl.find clauses_table (string_of_int i))
    else

      is_neq (negate_clause (Hashtbl.find clauses_table (string_of_int (-i))))
  in
  (* we go through the string model, when we find v or a space we ignore it and when we find a positive or negative integer we add the clauses *)

  let clauses = ref [] in

  let m = String.sub model 2 ( String.length model -4) in (* on récupère les valeurs du model *)
  let m' = String.split_on_char ' ' m in 
  let val_model = List.map ( fun s -> int_of_string s ) m' in


  List.iter ( fun i -> clauses := !clauses @ (ind_to_clauses i ) )  val_model ;

  List.iter (fun c -> problem := !problem ^ "\t" ^ c ^ "\n") !clauses ;
  problem := !problem ^"end";

  write "problem.lp" !problem;
  Lp.read "problem.lp"


(* modification du fichier .cnf *)

(* on crée la clause à rajouter, 
  on modifie le fichier cnf :  l'envoie dans un fichier auxiliaire pour traiter, puis on copie dans le fichier utilisé par glucose  *)

let negate_clause clause_str =
  let clause = String.trim clause_str |> String.split_on_char ' ' |> List.map int_of_string in
  let negation = List.map (fun x -> -x) clause in
  let negation_str = String.concat " " (List.map string_of_int negation) in
  negation_str

let copy_file source_file destination_file =
  let in_channel = open_in source_file in
  let out_channel = open_out destination_file in
  let rec loop () =
    match input_line in_channel with
    | exception End_of_file ->
      close_in in_channel;
      close_out out_channel
    | line ->
      output_string out_channel (line ^ "\n");
      loop ()
  in
  loop ()

let add_clause_to_file filename file_aux clause =
  let in_channel = open_in filename in
  let out_channel = open_out file_aux in
  let rec loop nb_vars nb_clauses () =
    match input_line in_channel with
    | exception End_of_file ->
      Printf.fprintf out_channel "%s 0\n" clause;
      close_in in_channel;
      close_out out_channel
    | line ->
      if String.length line > 0 && line.[0] = 'p' then
        let nb_vars', nb_clauses' = Scanf.sscanf line "p cnf %d %d" (fun v c -> (v, c)) in
        Printf.fprintf out_channel "p cnf %d %d\n" nb_vars' (nb_clauses' + 1)
      else
        output_string out_channel (line ^ "\n");
      loop nb_vars nb_clauses ()
  in
  loop 0 0 () ; 
  copy_file file_aux filename


(* implémentation de l'algorithme 3*)

let sublist l start length = 

    let rec aux liste length indice = 
      match liste with 
        [] -> if length > 0 then (print_int length ; failwith "failed sub_list") else []
        | t :: q when indice < start -> aux q length (indice+1)
        | t :: q -> if length > 0 then [t] @ ( aux q (length -1) (indice+1) ) else []
    in aux l length 0


let rec dichotomie_iteree liste_inc liste_reste ht = (* à corriger *)

  let solve_problem problem =
          Lp_glpk.solve ~term_output:false problem 
    in 

    let result_is_ok problem = 
      match solve_problem problem with
          | Ok (obj, xs) -> true
          | Error msg -> false
    in

  let gauche = ref 0 in 
  let droite = ref (List.length (liste_inc @ liste_reste )-1) in 

  let m = ref ((!droite + !gauche) / 2) in
  let model_m = ref (sublist (liste_inc @ liste_reste ) 0 (!m+1)) in 
  let str_model_m = ref ("v "^(String.concat " " !model_m)^" 0") in

  let problem = ref (initialize_problem (!str_model_m) ht) in

  while (!droite - !gauche > 1) do

    m := (!droite + !gauche) / 2 ;
    model_m := sublist (liste_inc @ liste_reste ) 0 (!m+1) ;
    str_model_m := "v "^(String.concat " " !model_m)^" 0" ;

    problem := (initialize_problem (!str_model_m) ht) ;

    if result_is_ok !problem then gauche := !m else droite := !m ;
  done ;

  m := (!droite + !gauche) / 2 ;

  model_m := sublist (liste_inc @ liste_reste ) 0 (!m+1) ;
  str_model_m := "v "^(String.concat " " !model_m)^" 0" ;
  problem := (initialize_problem (!str_model_m) ht) ;


  let c_m = if (result_is_ok !problem) then sublist (liste_inc @ liste_reste ) !droite 1  else sublist (liste_inc @ liste_reste ) !gauche 1 in
  let ind_cm = if (result_is_ok !problem) then !droite  else !gauche in

  let result = 
  if ( (ind_cm) <= List.length liste_inc -1 ) (* on a raffiné au max *)
    then liste_inc
  else (

    let gauche' = ref 0 in 
    let droite' = ref (ind_cm - List.length liste_inc) in 

    let m' = ref ((!droite' + !gauche') / 2) in
    let model_m' = ref (liste_inc @ (sublist liste_reste 0 (!m'+1)) ) in 
    let str_model_m' = ref ("v "^(String.concat " " !model_m')^" 0") in

    let problem' = ref (initialize_problem !str_model_m' ht) in

    while (!droite' - !gauche' > 1) do 

      m' := (!droite' + !gauche') / 2 ;
      model_m' := (liste_inc @ (sublist liste_reste 0 (!m'+1)) ) ;
      str_model_m' := "v "^(String.concat " " !model_m')^" 0" ;

      problem' := initialize_problem !str_model_m' ht ;

      gauche' := if result_is_ok !problem' then !gauche' else !m' ;
      droite' := if result_is_ok !problem' then !m' else !droite' ;
    done ;

    m' := (!droite' + !gauche') / 2 ;
    model_m' := (liste_inc @ (sublist liste_reste 0 (!m'+1)) ) ;
    str_model_m' := "v "^(String.concat " " !model_m')^" 0" ;
    problem' := initialize_problem !str_model_m' ht ;

    let c_k = if (result_is_ok !problem') then sublist (liste_reste ) !gauche' 1  else sublist (liste_reste) !droite' 1 in 
    let ind_ck = if (result_is_ok !problem') then !gauche' else !droite' in 

    if ( ind_ck = ind_cm - List.length liste_inc ) 
      then liste_inc @ c_m
    else (

      let new_reste = (sublist liste_reste 0 ind_ck) @ (sublist liste_reste (ind_ck+1) (ind_cm - List.length liste_inc - ind_ck -1 ) ) in
      dichotomie_iteree (liste_inc @ c_k @ c_m )  new_reste ht
    )
   )
in result




(* il faut prend model sous la forme : "1 2 3 ... n " *)

let s_i_iteree model_val ht =
  let model = String.split_on_char ' ' model_val in

  (* on ne fait pas ici l'optimisation avec les k_i et m_i *)
  let n = List.length model -1 in
  let i = ref 0 in   
  let s_min = ref model in

  while !i <= n do 
    let s_i = dichotomie_iteree [] ( (sublist model !i (n- !i +1)) @ (sublist model 0 !i)) ht  in (* pour la liste circulaire *)

    i := !i +1 ;
    if (List.length s_i) < (List.length !s_min) 
        then ( 
            s_min := s_i ; 
            )
  done ;
  print_string "\n" ;
  !s_min




(* main *)


let main () =
  let args = Sys.argv in
  if Array.length args < 2 then
    Printf.printf "Usage: %s <input_file> \n" args.(0)
  else

    let input_file = args.(1) in
    let output_file = "out.cnf" in

    try 
      
      (* transformation de la formule de base en clauses et hashtable *)
      let s = read_txt input_file in
      print_string ( "Formule : "^s ^"\n\n" ) ; 
      let s' = List.rev( process s) in
      let ht,f = hash s' in

      erase_parenthesis ht ; (* car le parseur de Lp aime pas *)
      
      (* pour le debuggage *)
      (*
      let print_hashtable ht =
      Hashtbl.iter (fun k v -> Printf.printf "%s -> %s\n" k v) ht  
      in print_hashtable ht ;
      *)

      (* ecriture en cnf dimacs *)
      output output_file f ;

      (* appel à glucose *)
      let status, model = run_glucose output_file in

      let stat = ref status in 
      let model_val = ref model in 

      let problem = ref (initialize_problem !model_val ht) in
     


(*  corps de la fonction d'échange entre les programmes *) 

      let cond = ref false in

       while (!stat <> "s UNSATISFIABLE") && ( not !cond )  do 

            (* appel à glpk *)
            
            problem := initialize_problem !model_val ht ;
          
            let solve_problem problem =
              Lp_glpk.solve ~term_output:false problem 
            in 

            match solve_problem !problem with

               | Ok (obj, xs) -> 

                    cond := true ;
                    print_string "Voici une valuation convenant pour cette formule \n" ;

                    let vars = Lp.Problem.take_vars !problem in 
                    let rec aux_print_val list_var = 
                      match list_var with 
                        | [] -> ()
                        | t :: q -> 
                                    Printf.printf "%.s : %.2f \n" ( Lp.Var.to_string t ) (Lp.PMap.find (Lp.Poly.of_var t) xs) ;
                                    aux_print_val q ;
                    in aux_print_val vars ;
                    

               | Error msg -> 

                    (* ajout de la nouvelle clause dans le output file *) 
                    let clause_add = negate_clause (String.sub !model_val 2 (String.length !model_val -4)) in
                    add_clause_to_file output_file "aux.cnf" clause_add ;

                    let (status', model') = run_glucose output_file in
                    stat := status' ;
                    model_val := model' ;
                     

      done ; 

      if !stat = "s UNSATISFIABLE" 
          then print_string "La formule n'est pas satisfiable \n"


      
    with
    | Sys_error msg -> Printf.printf "Error: %s\n" msg
    | exn -> Printf.printf "Error: %s\n" (Printexc.to_string exn)


let main2 () = 

  let args = Sys.argv in
  if Array.length args < 2 then
    Printf.printf "Usage: %s <input_file> \n" args.(0)
  else

    let input_file = args.(1) in
    let output_file = "out.cnf" in

    try 
      
      (* transformation de la formule de base en clauses et hashtable *)
      let s = read_txt input_file in
      print_string ( "Formule : "^s ^"\n" ) ; 
      let s' = List.rev( process s) in
      let ht,f = hash s' in

      erase_parenthesis ht ; (* car le parseur de Lp aime pas *)
      
      (* pour le debuggage *)
      (*
      let print_hashtable ht =
      Hashtbl.iter (fun k v -> Printf.printf "%s -> %s\n" k v) ht  
      in print_hashtable ht ;
      *)

      (* ecriture en cnf dimacs *)
      output output_file f ;

      (* appel à glucose *)
      let status, model = run_glucose output_file in
      let model_clean = String.sub model 2 (String.length model -4) in

      let res = String.concat " " (s_i_iteree model_clean ht) in
      print_string ("S_min : " ^res ^"\n")

    with
      | Sys_error msg -> Printf.printf "Error: %s\n" msg
      | exn -> Printf.printf "Error: %s\n" (Printexc.to_string exn)

let () = main2 ()


