module System = struct
  let fresh_int =
    let counter = ref ~-1 in
    let body () =
      counter := !counter + 1 ;
      !counter in
    body

  type nat = Z | S of nat | V of int
  type op = Plus | Times
  type bin_op = BinOp of op * nat * nat
  type judgement = J_is of bin_op * nat
  type rule = P_Zero | P_Succ | T_Zero | T_Succ

  let inversion = function
    | J_is (BinOp (Plus, Z, n), n') -> (P_Zero, [], [(n, n')])
    | J_is (BinOp (Plus, S n1, n2), n') ->
        let n = V (fresh_int ()) in
        (P_Succ, [J_is (BinOp (Plus, n1, n2), n)], [(S n, n')])
    | J_is (BinOp (Times, Z, _), n') -> (T_Zero, [], [(n', Z)])
    | J_is (BinOp (Times, S n1, n2), n4) ->
        let n3 = V (fresh_int ()) in
        ( T_Succ
        , [J_is (BinOp (Times, n1, n2), n3); J_is (BinOp (Plus, n2, n3), n4)]
        , [] )
    | J_is (BinOp ((Plus | Times), V _, _), _) -> assert false

  let result_of_judgement = function J_is (_, n) -> n

  module String = struct
    let rec of_nat = function
      | Z -> "Z"
      | S n -> "S(" ^ of_nat n ^ ")"
      | V i -> "n" ^ string_of_int i

    let of_op = function Plus -> "plus" | Times -> "times"

    let of_bin_op = function
      | BinOp (op, n1, n2) -> of_nat n1 ^ " " ^ of_op op ^ " " ^ of_nat n2

    let of_judgement = function
      | J_is (bin_op, n) -> of_bin_op bin_op ^ " is " ^ of_nat n

    let of_rule = function
      | P_Zero -> "P-Zero"
      | P_Succ -> "P-Succ"
      | T_Zero -> "T-Zero"
      | T_Succ -> "T-Succ"
  end

  let string_of_judgement = String.of_judgement
  let string_of_rule = String.of_rule
end

module Tree = Core.Derivation_tree.Make (System)
open System
open Tree

type subst = (int * nat) list

let rec one_subst_nat (i, n) = function
  | Z -> Z
  | S n' -> S (one_subst_nat (i, n) n')
  | V i' -> if i = i' then n else V i'

let subst_nat subst nat =
  List.fold_left (fun nat' s -> one_subst_nat s nat') nat subst

let one_subst_judgement s = function
  | J_is (BinOp (op, n1, n2), n3) ->
      let n1 = one_subst_nat s n1 in
      let n2 = one_subst_nat s n2 in
      let n3 = one_subst_nat s n3 in
      J_is (BinOp (op, n1, n2), n3)

let subst_judgement subst judgement =
  List.fold_left (fun ty' s -> one_subst_judgement s ty') judgement subst

let rec one_subst_tree s = function
  | Tree (j, r, st) ->
      Tree (one_subst_judgement s j, r, List.map (one_subst_tree s) st)

let subst_tree subst tree =
  List.fold_left (fun ty' s -> one_subst_tree s ty') tree subst

let eqs_of_subst subst = List.rev_map (fun (i, n) -> (V i, n)) subst

let rec subst_eqs subst eqs =
  match subst with
  | [] -> eqs
  | s :: rest ->
      let f n = one_subst_nat s n in
      let eqs' = List.rev_map (fun (n1, n2) -> (f n1, f n2)) eqs in
      subst_eqs rest eqs'

let rec of_nat = function
  | Z -> "Z"
  | S n -> "S(" ^ of_nat n ^ ")"
  | V i -> "n" ^ string_of_int i

let rec unify = function
  | [] -> []
  | (n, n') :: eqs -> (
      if n = n' then unify eqs
      else
        match (n, n') with
        | S n, S n' -> unify ((n, n') :: eqs)
        | n, V i | V i, n -> (i, n) :: unify (subst_eqs [(i, n)] eqs)
        | _ -> assert false )
    

let rec derive judgement =
  let rule, sub_judgement_list, eqs = inversion judgement in
  let rec aux subst = function
    | [] -> ([], subst)
    | j' :: sj' -> (
        let tree, subst = derive (subst_judgement subst j') in
        match aux subst sj' with ts, subst -> (tree :: ts, subst) ) in
  let sub_tree_list, subst = aux [] sub_judgement_list in
  let eqs = eqs @ eqs_of_subst subst in
  let subst = unify eqs in
  (Tree (subst_judgement subst judgement, rule, sub_tree_list), subst)

let derive j = match derive j with t, _ -> t
