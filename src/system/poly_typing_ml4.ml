module System = struct
  let to_var alpha = int_of_char (String.get alpha 0) - int_of_char 'a'

  type op = Plus | Minus | Times | Lt

  type var = string

  type exp =
    | E_int of int
    | E_bool of bool
    | E_var of var
    | BinOp of op * exp * exp
    | E_if of exp * exp * exp
    | E_let of var * exp * exp
    | E_fun of var * exp
    | E_app of exp * exp
    | E_letrec of var * var * exp * exp
    | E_nil
    | E_cons of exp * exp
    | E_match of exp * exp * var * var * exp

  type type_ =
    | T_bool
    | T_int
    | T_fun of type_ * type_
    | T_list of type_
    | V of int

  let fresh_int =
    let counter = ref 10 in
    let body () =
      counter := !counter + 1;
      !counter
    in
    body

  let new_V () = V (fresh_int ())

  type tysc = int list * type_

  type env = (var * tysc) list

  type judgement = J_typing of env * exp * type_

  type rule =
    | T_Int
    | T_Bool
    | T_If
    | T_Plus
    | T_Minus
    | T_Times
    | T_Lt
    | T_Var
    | T_Let
    | T_Fun
    | T_App
    | T_LetRec
    | T_Nil
    | T_Cons
    | T_Match

  let result_of_judgement = function J_typing (_, _, t) -> t

  let rule_of_op = function
    | Plus -> T_Plus
    | Minus -> T_Minus
    | Times -> T_Times
    | Lt -> T_Lt

  module String = struct
    let of_op = function Plus -> "+" | Minus -> "-" | Times -> "*" | Lt -> "<"

    (* int -> int *)
    (* var -> char *)
    (*
         let cnt = ref 0

         let rec rename_type l = function
           | T_bool | T_int -> l
           | T_fun (t1, t2) -> rename_type (rename_type l t1) t2
           | T_list t -> rename_type l t
           | V i ->
               if List.mem_assoc i l then l
               else
                 let x = (i, !cnt) in
                 cnt := !cnt + 1;
                 x :: l

         let rec rename_env l = function
           | [] -> l
           | (_, (_, t)) :: env -> rename_env (rename_type l t) env

         let rename_env l env = rename_env l (List.rev env) *)

    let rec of_type = function
      | T_bool -> "bool"
      | T_int -> "int"
      | T_fun (t1, t2) ->
          let s1 =
            match t1 with T_fun _ -> "(" ^ of_type t1 ^ ")" | _ -> of_type t1
          in
          s1 ^ " -> " ^ of_type t2
      | T_list t ->
          let s =
            match t with T_fun _ -> "(" ^ of_type t ^ ")" | _ -> of_type t
          in
          s ^ " list"
      | V i ->
          let j = i / 26 in
          let n = if j = 0 then "" else string_of_int j in
          "'" ^ Char.escaped (char_of_int (int_of_char 'a' + (i mod 26))) ^ n
    (* (char_of_int (int_of_char 'a' + List.assoc i id_num_list)) *)

    let rec of_vars = function
      | [] -> []
      | i :: vars ->
          let j = i / 26 in
          let n = if j = 0 then "" else string_of_int j in
          ("'" ^ Char.escaped (char_of_int (int_of_char 'a' + (i mod 26))) ^ n)
          :: of_vars vars

    let of_tysc (vars, t) =
      (* let id_num_list = rename t in
         let vars =
           List.map
             (fun i ->
               try List.assoc i id_num_list
               with _ ->
                 print_endline (String.concat " " (of_vars vars));
                 print_endline (of_type id_num_list t);
                 assert false)
             vars
         in *)
      let q =
        match vars with
        | [] -> ""
        | vars -> String.concat " " (of_vars vars) ^ "."
      in
      q ^ of_type t
    (* q ^ of_type id_num_list t *)

    (* let of_type t = of_type (rename t) t *)

    let rec paren_special = function
      | ( E_if _ | E_let _ | E_fun _ | E_app _ | E_letrec _ | E_cons _
        | E_match _ ) as e ->
          "(" ^ of_exp e ^ ")"
      | e -> of_exp e

    and of_exp = function
      | E_int i -> string_of_int i
      | E_bool b -> string_of_bool b
      | E_var x -> x
      | BinOp (Lt, e1, e2) ->
          paren_special e1 ^ " " ^ of_op Lt ^ " " ^ of_exp e2
      | BinOp (((Plus | Minus) as op), e1, e2) ->
          let s2 =
            match e2 with
            | BinOp ((Plus | Minus), _, _) -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2
          in
          paren_special e1 ^ " " ^ of_op op ^ " " ^ s2
      | BinOp (Times, e1, e2) ->
          let s1 =
            match e1 with
            | BinOp ((Plus | Minus), _, _) -> "(" ^ of_exp e1 ^ ")"
            | _ -> paren_special e1
          in
          let s2 =
            match e2 with
            | BinOp (_, _, _) -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2
          in
          s1 ^ " " ^ of_op Times ^ " " ^ s2
      | E_if (e1, e2, e3) ->
          "if " ^ of_exp e1 ^ " then " ^ of_exp e2 ^ " else " ^ of_exp e3
      | E_let (x, e1, e2) -> "let " ^ x ^ " = " ^ of_exp e1 ^ " in " ^ of_exp e2
      | E_fun (x, e) -> "fun " ^ x ^ " -> " ^ of_exp e
      | E_app (e1, e2) ->
          let wrap = function
            | (E_var _ | E_int _ | E_bool _) as e -> of_exp e
            | e -> "(" ^ of_exp e ^ ")"
          in
          wrap e1 ^ " " ^ wrap e2
      | E_letrec (f, x, e1, e2) ->
          "let rec " ^ f ^ " = fun " ^ x ^ " -> " ^ of_exp e1 ^ " in "
          ^ of_exp e2
      | E_nil -> "[]"
      | E_cons (e1, e2) ->
          let s1 =
            match e1 with
            | E_cons _ -> "(" ^ of_exp e1 ^ ")"
            | _ -> paren_special e1
          in
          s1 ^ " :: " ^ of_exp e2
      | E_match (e1, e2, x, y, e3) ->
          "match " ^ of_exp e1 ^ " with [] -> " ^ of_exp e2 ^ " | " ^ x ^ " :: "
          ^ y ^ " -> " ^ of_exp e3

    and of_env = function
      | [] -> ""
      | [ (x, t) ] -> x ^ " : " ^ of_tysc t
      | (x, t) :: env -> of_env env ^ ", " ^ x ^ " : " ^ of_tysc t

    let of_judgement = function
      | J_typing (env, e, t) ->
          of_env env ^ " |- " ^ of_exp e ^ " : " ^ of_type t

    let of_rule = function
      | T_Int -> "T-Int"
      | T_Bool -> "T-Bool"
      | T_If -> "T-If"
      | T_Plus -> "T-Plus"
      | T_Minus -> "T-Minus"
      | T_Times -> "T-Mult"
      | T_Lt -> "T-Lt"
      | T_Var -> "T-Var"
      | T_Let -> "T-Let"
      | T_Fun -> "T-Abs"
      | T_App -> "T-App"
      | T_LetRec -> "T-LetRec"
      | T_Nil -> "T-Nil"
      | T_Cons -> "T-Cons"
      | T_Match -> "T-Match"
  end

  let string_of_judgement = String.of_judgement

  let string_of_rule = String.of_rule
end

module Tree = Core.Derivation_tree.Make (System)
open System
open Tree

type eqs = type_ * type_

type subst = int * type_

type substs = subst list

let eqs_of_substs subst = List.rev_map (fun (alpha, t) -> (V alpha, t)) subst

let rec one_subst_type (alpha, t) = function
  | (T_bool | T_int) as t' -> t'
  | T_fun (t1, t2) ->
      T_fun (one_subst_type (alpha, t) t1, one_subst_type (alpha, t) t2)
  | T_list t' -> T_list (one_subst_type (alpha, t) t')
  | V beta -> if alpha = beta then t else V beta

let subst_type substs ty =
  List.fold_left (fun ty' subst -> one_subst_type subst ty') ty substs

let rec unify = function
  | [] -> []
  | (t, t') :: eqs -> (
      if t = t' then unify eqs
      else
        match (t, t') with
        | T_fun (t1, t2), T_fun (t1', t2') ->
            unify ((t1, t1') :: (t2, t2') :: eqs)
        | T_list t, T_list t' -> unify ((t, t') :: eqs)
        | t, V i | V i, t ->
            let f = one_subst_type (i, t) in
            (i, t) :: unify (List.rev_map (fun (t1, t2) -> (f t1, f t2)) eqs)
        | _ -> assert false)

let subst_vars substs =
  List.map (fun i ->
      match subst_type substs (V i) with V j -> j | _ -> assert false)

let subst_tysc substs = function
  | vars, ty -> (subst_vars substs vars, subst_type substs ty)

let subst_env substs = List.map (fun (x, tysc) -> (x, subst_tysc substs tysc))

let subst_judgement substs = function
  | J_typing (env, e, t) ->
      let env = subst_env substs env in
      let t = subst_type substs t in
      J_typing (env, e, t)

let rec subst_tree substs = function
  | Tree (j, r, ts) ->
      let j = subst_judgement substs j in
      Tree (j, r, List.map (fun t -> subst_tree substs t) ts)

let rec union l1 = function
  | [] -> l1
  | x :: l2 -> if List.mem x l1 then union l1 l2 else union (x :: l1) l2

let bigunion ls = List.fold_left (fun u l -> union u l) [] ls

let rec freevar_ty = function
  | T_int | T_bool -> []
  | T_fun (t1, t2) -> union (freevar_ty t1) (freevar_ty t2)
  | T_list t -> freevar_ty t
  | V alpha -> [ alpha ]

let rec set_minus l1 l2 =
  match l1 with
  | [] -> []
  | x :: l1 -> if List.mem x l2 then set_minus l1 l2 else x :: set_minus l1 l2

let freevar_tysc (tyvar_list, ty) = set_minus (freevar_ty ty) tyvar_list

let freevar_tyenv tyenv =
  List.fold_left (fun vars (_, tysc) -> union vars (freevar_tysc tysc)) [] tyenv

let closure ty tyenv subst =
  let ty = subst_type subst ty in
  let fv_tyenv' = freevar_tyenv tyenv in
  let fv_tyenv =
    bigunion
      (List.map (fun id -> freevar_ty (subst_type subst (V id))) fv_tyenv')
  in
  let ids = set_minus (freevar_ty ty) fv_tyenv in
  let s = List.map (fun id -> (id, new_V ())) ids in
  (subst_vars s ids, subst_type s ty)

let rec derive_type e env =
  match e with
  | E_int _ -> (Tree (J_typing (env, e, T_int), T_Int, []), [])
  | E_bool _ -> (Tree (J_typing (env, e, T_bool), T_Bool, []), [])
  | E_if (e1, e2, e3) ->
      let tr1, s1 = derive_type e1 env in
      let tr2, s2 = derive_type e2 env in
      let tr3, s3 = derive_type e3 env in
      let t1 = result_of_judgement (root_of_tree tr1) in
      let t2 = result_of_judgement (root_of_tree tr2) in
      let t3 = result_of_judgement (root_of_tree tr3) in
      let eqs =
        [ (t1, T_bool); (t2, t3) ] @ eqs_of_substs s1 @ eqs_of_substs s2
        @ eqs_of_substs s3
      in
      let substs = unify eqs in
      ( Tree (J_typing (env, e, subst_type substs t2), T_If, [ tr1; tr2; tr3 ]),
        substs )
  | BinOp (op, e1, e2) ->
      let tr1, s1 = derive_type e1 env in
      let tr2, s2 = derive_type e2 env in
      let t1 = result_of_judgement (root_of_tree tr1) in
      let t2 = result_of_judgement (root_of_tree tr2) in
      let t = if op = Lt then T_bool else T_int in
      let eqs =
        [ (t1, T_int); (t2, T_int) ] @ eqs_of_substs s1 @ eqs_of_substs s2
      in
      let substs = unify eqs in
      (Tree (J_typing (env, e, t), rule_of_op op, [ tr1; tr2 ]), substs)
  | E_var x ->
      let vars, t =
        try List.assoc x env
        with _ ->
          print_endline "a";
          assert false
      in
      let s = List.map (fun id -> (id, new_V ())) vars in
      (Tree (J_typing (env, e, subst_type s t), T_Var, []), [])
  | E_let (x, e1, e2) ->
      let tr1, s1 = derive_type e1 env in
      let t1 = result_of_judgement (root_of_tree tr1) in
      let tysc = closure t1 env s1 in
      let env' = (x, tysc) :: env in
      let tr2, s2 = derive_type e2 env' in
      let t2 = result_of_judgement (root_of_tree tr2) in
      let eqs = eqs_of_substs s1 @ eqs_of_substs s2 in
      let substs = unify eqs in
      ( Tree (J_typing (env, e, subst_type substs t2), T_Let, [ tr1; tr2 ]),
        substs )
  | E_fun (x, e') ->
      let t1 = new_V () in
      let env' = (x, ([], t1)) :: env in
      let tr, substs = derive_type e' env' in
      let t2 = result_of_judgement (root_of_tree tr) in
      let t = T_fun (subst_type substs t1, t2) in
      (Tree (J_typing (env, e, t), T_Fun, [ tr ]), substs)
  | E_app (e1, e2) ->
      let tr1, s1 = derive_type e1 env in
      let tr2, s2 = derive_type e2 env in
      let t1 = result_of_judgement (root_of_tree tr1) in
      let t2 = result_of_judgement (root_of_tree tr2) in
      let eqs, t =
        match t1 with
        | T_fun (t2', t3') -> ([ (t2', t2) ], t3')
        | V alpha ->
            let t3 = new_V () in
            ([ (V alpha, T_fun (t2, t3)) ], t3)
        | _ -> assert false
      in
      let eqs = eqs @ eqs_of_substs s1 @ eqs_of_substs s2 in
      let substs = unify eqs in
      (Tree (J_typing (env, e, t), T_App, [ tr1; tr2 ]), substs)
  | E_letrec (x, y, e1, e2) ->
      let t1 = new_V () in
      let t2 = new_V () in
      let t12 = T_fun (t1, t2) in
      let env1 = (y, ([], t1)) :: (x, ([], t12)) :: env in
      let tr1, s1 = derive_type e1 env1 in
      let tysc = closure t12 env s1 in
      let env2 = (x, tysc) :: env in
      let tr2, s2 = derive_type e2 env2 in
      let eqs = eqs_of_substs s1 @ eqs_of_substs s2 in
      let substs = unify eqs in
      let t = result_of_judgement (root_of_tree tr2) in
      ( Tree (J_typing (env, e, subst_type substs t), T_LetRec, [ tr1; tr2 ]),
        substs )
  | E_nil ->
      let t = T_list (new_V ()) in
      (Tree (J_typing (env, e, t), T_Nil, []), [])
  | E_cons (e1, e2) ->
      let tr1, s1 = derive_type e1 env in
      let tr2, s2 = derive_type e2 env in
      let t1 = result_of_judgement (root_of_tree tr1) in
      let t2 = result_of_judgement (root_of_tree tr2) in
      let eqs = [ (T_list t1, t2) ] @ eqs_of_substs s1 @ eqs_of_substs s2 in
      let substs = unify eqs in
      (Tree (J_typing (env, e, t2), T_Cons, [ tr1; tr2 ]), substs)
  | E_match (e1, e2, x, y, e3) ->
      let tr1, s1 = derive_type e1 env in
      let tr2, s2 = derive_type e2 env in
      let t1 = result_of_judgement (root_of_tree tr1) in
      let t2 = result_of_judgement (root_of_tree tr2) in
      let t' = new_V () in
      let env3 = (y, ([], T_list t')) :: (x, ([], t')) :: env in
      let tr3, s3 = derive_type e3 env3 in
      let t3 = result_of_judgement (root_of_tree tr3) in
      let eqs =
        [ (t2, t3); (t1, T_list t') ]
        @ eqs_of_substs s1 @ eqs_of_substs s2 @ eqs_of_substs s3
      in
      let substs = unify eqs in
      (Tree (J_typing (env, e, t2), T_Match, [ tr1; tr2; tr3 ]), substs)

let derive = function
  | J_typing (env, e, _) -> (
      (* match derive_type e env with tree, _-> tree) *)
      match derive_type e env with tree, substs -> subst_tree substs tree)
