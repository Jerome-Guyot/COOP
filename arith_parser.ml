(* the goal is to parse an arithmetic expretion of the form left op right, with op eq, lt, gt, leq, geq *)
(* the left and right part can be a variable, a constant or an arithmetic expression *)
(* the goal is to parse the string into a Lp expression *)

(* the lexemes are the different parts of the string *)

open Lp

(* Counter for variables id *)
let var_count = ref 0
(* precision of the strict inequations *)
let epsilon = 0.00001

type lexeme =
	|Pao
	|Paf
	|LInt of string
  |LVar of string
	|LAdd
	|LSub
	|LMul
	|LDiv

(* aexp is an arithmetic expression *)
type aexp =
  | AVar of string
  | AInt of int
  | Add of aexp * aexp
  | Sub of aexp * aexp
  | Mul of aexp * aexp
  | Div of aexp * aexp

let string_to_list f =
  let rec aux i l =
    if i<0 then l else aux (i-1) (f.[i] :: l)
  in aux (String.length f - 1) [];;

let rec anal_lex f =
  match f with
  | [] -> []
  | ' ' :: f1 -> anal_lex f1
  | '(' :: f1 -> Pao :: (anal_lex f1)
  | ')' :: f1 -> Paf :: (anal_lex f1)
  | '+':: f1 -> LAdd :: (anal_lex f1)
  | '-' :: f1 -> LSub :: (anal_lex f1)
  | '*' :: f1 -> LMul :: (anal_lex f1)
  | '/' :: f1 -> LDiv :: (anal_lex f1)
  (* on extrait la partie droite à laquelle on rajoute le caractère courant*)
  | t1 :: t2 :: f1
      when t1 <= '9' && t1 >= '0' && t2 <= '9' && t2 >= '0' -> (
      match anal_lex (t2::f1) with
      | LInt str :: q ->
        (LInt ((String.make 1 t1) ^ str)) :: q
      | _ -> failwith "ERROR : invalid int"
      )
  (*si le 1er caractère est un chiffre, et qu'on arrive au traitement de ce cas, alors*)
  (*le suivant n'en est pas un, et on sait que le caractère courant est tout à droite *)
  | t1 :: f1 when t1 <= '9' && t1 >= '0' ->
      LInt (String.make 1 t1) :: (anal_lex f1)
  (* variable extraction : if letter and the rest is an int then letter ^ int, else letter ^ variable, and if letter alone then variable *)
  | t1 :: t2 :: f1
      when t1 <= 'z' && t1 >= 'a' &&
        ((t2 <= '9' && t2 >= '0' ) || (t2 <= 'z' && t2 >= 'a')) -> (
      match anal_lex (t2::f1) with
      | LInt str :: q ->
        (LVar ((String.make 1 t1) ^ str)) :: q
      | LVar str :: q ->
        (LVar ((String.make 1 t1) ^ str)) :: q
      | _ -> failwith "ERROR : invalid var"
      )
  | t1 :: f1 when t1 <= 'z' && t1 >= 'a' ->
      LVar (String.make 1 t1) :: (anal_lex f1)
  | _	-> failwith "Il y a un caractère indésirable : "

let extract_bracket lexemes =
  let rec aux left right n =
    match right with
    | [] -> failwith "ERROR : emptied the list without finding a matching closing bracket"
    | Pao :: q ->
      aux (Pao :: left) q (n+1)
    | Paf :: q ->
      print_int(n);
      if n = 0 then List.rev left, q else aux (Paf :: left) q (n-1)
    | t :: q ->
      aux (t :: left) q n
  in aux [] lexemes 0

let op_to_op op =
  match op with
  | LAdd -> Add (AVar "poubelle", AVar "poubelle")
  | LSub -> Sub (AVar "poubelle", AVar "poubelle")
  | LMul -> Mul (AVar "poubelle", AVar "poubelle")
  | LDiv -> Div (AVar "poubelle", AVar "poubelle")
  | _ -> failwith "ERROR : invalid op"

let rec find_term lexemes =
  match lexemes with
  | [] -> failwith "ERROR : empty list"
  | LInt str :: q -> AInt (int_of_string str), q
  | LVar str :: q -> AVar str, q
  | Pao :: q ->
      let term, rest = extract_bracket q in
      parse_arith term, rest
  | LSub :: q ->
      let term, rest = find_term q in
      Sub (AInt 0, term), rest
  | _ -> failwith "ERROR : invalid term"

and lexeme_to_term lexemes =
  match lexemes with
  | [] -> []
  | _ -> 
    let term, rest = find_term lexemes in
    match rest with
    | [] -> [term]
    | op :: q -> term :: (op_to_op op) :: (lexeme_to_term q)

(*
  (* convert the string to a list of chars *)
  let str_list = string_to_list str in
  (* analyse the list of chars *)
  let lex_list = anal_lex str_list in
*)

and parse_arith lex_list =
  (* convert the list of lexemes to a list of terms *)
  let term_list = lexeme_to_term lex_list in
  let rev_list = List.rev term_list in

  (* we find the first additon or substraction, and we parse the left and right part *)
  (* if no such operation is found, we do the same with multiplication and division *)
  (* if no such operation is found, we return the term *)
  
  let rec find_op left right priority =
    (* priority 0 : + - *)
    (* priority 1 : * / *)
    match right with
    | [term] ->
        if priority = 0 then
          find_op [] (left @ [term]) 1
        else (
          match left with
          | [] -> term
          | _ -> failwith "ERROR : no operation found"
        )
    | t1 :: op :: q when priority = 0 && (op = Add (AVar "poubelle", AVar "poubelle") || op = Sub (AVar "poubelle", AVar "poubelle")) ->
        let left_exp = find_op [] (left @ [t1]) 1 in
        let right_exp = find_op [] q 0 in (
        (* find_op is initially called on the reversed list, so we need to flip left and right *)
        match op with
        | Add (AVar "poubelle", AVar "poubelle") -> Add (right_exp, left_exp)
        | Sub (AVar "poubelle", AVar "poubelle") -> Sub (right_exp, left_exp)
        | _ -> failwith "ERROR : not suposed to happen code 63841"
        )
    | t1 :: op :: q when priority = 1 && (op = Mul (AVar "poubelle", AVar "poubelle") || op = Div (AVar "poubelle", AVar "poubelle")) ->
        let left_exp = find_op [] (left @ [t1]) 1 in
        let right_exp = find_op [] q 1 in (
        match op with
        | Mul (AVar "poubelle", AVar "poubelle") -> Mul (right_exp, left_exp)
        | Div (AVar "poubelle", AVar "poubelle") -> Div (right_exp, left_exp)
        | _ -> failwith "ERROR : not suposed to happen code 35642"
        )
    | t1 :: op :: q ->
        find_op (left @ (t1 :: [op])) q priority
    | _ -> failwith "ERROR : invalid expression"
  in
  find_op [] rev_list 0

(* parse the string litteral into left op right *)
let parse_clause litteral =
  (* find the op *)
  let op, ind = 
    let rec find_op i =
      if i >= String.length litteral then failwith "no op found"
      else if litteral.[i] = '=' then "=", i
      else if litteral.[i] = '<' then 
        if i+1 < String.length litteral && litteral.[i+1] = '='
        then "<=", i
        else "<", i
      else if litteral.[i] = '>' then 
        if i+1 < String.length litteral && litteral.[i+1] = '='
        then ">=", i
        else ">", i
      else find_op (i+1)
    in find_op 0
  in
  (* split the string into left and right *)
  let left = String.sub litteral 0 ind in
  let right = String.sub litteral (ind + (String.length op)) (String.length litteral - (ind + (String.length op))) in
  (* parse the left and right *)
  let left_lex = anal_lex (string_to_list left) in
  let left_arith = parse_arith left_lex in
  let right_lex = anal_lex (string_to_list right) in
  let right_arith = parse_arith right_lex in
  
  (* now we create a list of constraints *)
  (* As GLPK doesn't support brackets, we introduce a new variable
     for each of them and we add the constraint that the new variable
     is equal to the expression between the brackets *)
  
  (* At this point, all the brackets are gone and we are left with a tree.
     So for every subtree, we create a new variable and we add the constraint
     that the new variable is equal to the subtree *)
  
  let constraints = ref [] in
    
  let rec bracket_elimination arith =
    let lp_1, lp_2 =
      match arith with
      | Add (a1, a2) | Sub (a1, a2) | Mul (a1, a2) | Div (a1, a2) ->
        let lp_subtree aexp =
          match aexp with
          | AInt n -> Lp.c (float_of_int n)
          | AVar name -> Lp.var name
          | _ ->
            let new_var = "auxiliary_var_" ^ (string_of_int !var_count) in
            var_count := !var_count + 1;
            let lp_aexp = bracket_elimination aexp in
            constraints := ((Lp.var new_var) =~ lp_aexp) :: !constraints;
            Lp.var new_var
        in
        let lp_a1 = lp_subtree a1 in
        let lp_a2 = lp_subtree a2 in
        lp_a1, lp_a2
      | AInt n -> Lp.c (float_of_int n), Lp.c 0.
      | AVar name -> Lp.var name, Lp.c 0.
    in
    match arith with
    | Add (a1, a2) -> lp_1 ++ lp_2
    | Sub (a1, a2) -> lp_1 -- lp_2
    | Mul (a1, a2) -> lp_1 *~ lp_2
    | Div (a1, a2) -> lp_1 /~ lp_2
    | AInt n -> lp_1
    | AVar name -> lp_1
  in
  let left_lp = bracket_elimination left_arith in
  let right_lp = bracket_elimination right_arith in
  let final_constraint =
    match op with
    | "=" -> Lp.eq left_lp right_lp
    | "<" -> Lp.lt left_lp (right_lp -- Lp.c epsilon)
    | "<=" -> Lp.lt left_lp right_lp
    | ">" -> Lp.gt left_lp (right_lp ++ Lp.c epsilon)
    | ">=" -> Lp.gt left_lp right_lp
    | _ -> failwith "ERROR : invalid operator"

  in
  constraints := final_constraint :: !constraints;
  
  (* we return the list of constraints *)

  !constraints


