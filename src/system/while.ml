module System = struct
  type var = string

  type store = (var * int) list

  type aop = Plus | Minus | Times

  type aexp = A_int of int | A_var of var | A_binop of aop * aexp * aexp

  type lop = And | Or

  type comp = Lt | Eq | Le

  type bexp =
    | B_bool of bool
    | B_not of bexp
    | B_binop of lop * bexp * bexp
    | B_comp of comp * aexp * aexp

  type com =
    | C_skip
    | C_assign of var * aexp
    | C_seq of com * com
    | C_if of bexp * com * com
    | C_while of bexp * com

  type judgement =
    | J_aexp of store * aexp * int
    | J_bexp of store * bexp * bool
    | J_changes of com * store * store

  type result = R_int of int | R_bool of bool | R_store of store

  type rule =
    | A_Const
    | A_Var
    | A_Plus
    | A_Minus
    | A_Times
    | B_Const
    | B_Not
    | B_And
    | B_Or
    | B_Lt
    | B_Eq
    | B_Le
    | C_Skip
    | C_Assign
    | C_Seq
    | C_IfT
    | C_IfF
    | C_WhileT
    | C_WhileF

  let result_of_judgement = function
    | J_aexp (_, _, i) -> R_int i
    | J_bexp (_, _, b) -> R_bool b
    | J_changes (_, _, s) -> R_store s

  let rule_of_aop = function
    | Plus -> A_Plus
    | Minus -> A_Minus
    | Times -> A_Times

  let rule_of_comp = function Lt -> B_Lt | Eq -> B_Eq | Le -> B_Le

  let rule_of_lop = function And -> B_And | Or -> B_Or

  let fun_of_aop = function Plus -> ( + ) | Minus -> ( - ) | Times -> ( * )

  let fun_of_lop = function And -> ( && ) | Or -> ( || )

  let fun_of_comp = function Lt -> ( < ) | Eq -> ( = ) | Le -> ( <= )

  module String = struct
    let of_aop = function Plus -> "+" | Minus -> "-" | Times -> "*"

    let rec of_aexp = function
      | A_int i -> string_of_int i
      | A_var x -> x
      | A_binop (aop, a1, a2) ->
          let s1 =
            match a1 with
            | A_binop (op1, _, _) ->
                if op1 < aop then "(" ^ of_aexp a1 ^ ")" else of_aexp a1
            | _ -> of_aexp a1
          in

          let s2 =
            match a2 with
            | A_binop (op2, _, _) ->
                if op2 <= aop then "(" ^ of_aexp a2 ^ ")" else of_aexp a2
            | _ -> of_aexp a2
          in
          s1 ^ " " ^ of_aop aop ^ " " ^ s2

    let of_lop = function And -> "&&" | Or -> "||"

    let of_comp = function Lt -> "<" | Eq -> "=" | Le -> "<="

    let rec of_bexp = function
      | B_bool b -> string_of_bool b
      | B_not b -> (
          "!"
          ^
          match b with
          | B_bool _ | B_not _ -> of_bexp b
          | _ -> "(" ^ of_bexp b ^ ")")
      | B_binop (lop, b1, b2) ->
          let s1 =
            match b1 with
            | B_binop (op1, _, _) ->
                if op1 < lop then "(" ^ of_bexp b1 ^ ")" else of_bexp b1
            | _ -> of_bexp b1
          in

          let s2 =
            match b2 with
            | B_binop (op2, _, _) ->
                if op2 <= lop then "(" ^ of_bexp b2 ^ ")" else of_bexp b2
            | _ -> of_bexp b2
          in

          s1 ^ " " ^ of_lop lop ^ " " ^ s2
      | B_comp (comp, a1, a2) ->
          let s1 = of_aexp a1 in
          let s2 = of_aexp a2 in
          s1 ^ " " ^ of_comp comp ^ " " ^ s2

    let rec of_store = function
      | [] -> ""
      | [ (x, i) ] -> x ^ " = " ^ string_of_int i
      | (x, i) :: store -> of_store store ^ ", " ^ x ^ " = " ^ string_of_int i

    let rec of_com = function
      | C_skip -> "skip"
      | C_assign (x, a) -> x ^ " := " ^ of_aexp a
      | C_seq (c1, c2) -> of_com c1 ^ "; " ^ of_com c2
      | C_if (b, c1, c2) ->
          "if " ^ of_bexp b ^ " then " ^ of_com c1 ^ " else " ^ of_com c2
      | C_while (b, c) -> "while (" ^ of_bexp b ^ ") do " ^ of_com c

    let of_judgement = function
      | J_aexp (store, aexp, i) ->
          of_store store ^ " |- " ^ of_aexp aexp ^ " evalto " ^ string_of_int i
      | J_bexp (store, bexp, b) ->
          of_store store ^ " |- " ^ of_bexp bexp ^ " evalto " ^ string_of_bool b
      | J_changes (com, store, store') ->
          of_com com ^ " changes " ^ of_store store ^ " to " ^ of_store store'

    let of_rule = function
      | A_Const -> "A-Const"
      | A_Var -> "A-Var"
      | A_Plus -> "A-Plus"
      | A_Minus -> "A-Minus"
      | A_Times -> "A-Times"
      | B_Const -> "B-Const"
      | B_Not -> "B-Not"
      | B_And -> "B-And"
      | B_Or -> "B-Or"
      | B_Lt -> "B-Lt"
      | B_Eq -> "B-Eq"
      | B_Le -> "B-Le"
      | C_Skip -> "C-Skip"
      | C_Assign -> "C-Assign"
      | C_Seq -> "C-Seq"
      | C_IfT -> "C-IfT"
      | C_IfF -> "C-IfF"
      | C_WhileT -> "C-WhileT"
      | C_WhileF -> "C-WhileF"
  end

  let string_of_judgement = String.of_judgement

  let string_of_rule = String.of_rule
end

module Tree = Core.Derivation_tree.Make (System)
open System
open Tree

let rec derive_aexp a store =
  match a with
  | A_int i -> Tree (J_aexp (store, a, i), A_Const, [])
  | A_var x -> Tree (J_aexp (store, a, List.assoc x store), A_Var, [])
  | A_binop (aop, a1, a2) -> (
      let t1 = derive_aexp a1 store in
      let t2 = derive_aexp a2 store in
      let f t = result_of_judgement (root_of_tree t) in
      match (f t1, f t2) with
      | R_int i1, R_int i2 ->
          let i3 = fun_of_aop aop i1 i2 in
          Tree (J_aexp (store, a, i3), rule_of_aop aop, [ t1; t2 ])
      | _ -> assert false)

let rec derive_bexp bexp store =
  match bexp with
  | B_bool b -> Tree (J_bexp (store, bexp, b), B_Const, [])
  | B_not b -> (
      let t = derive_bexp b store in
      match result_of_judgement (root_of_tree t) with
      | R_bool b -> Tree (J_bexp (store, bexp, not b), B_Not, [ t ])
      | _ -> assert false)
  | B_binop (lop, b1, b2) -> (
      let t1 = derive_bexp b1 store in
      let t2 = derive_bexp b2 store in
      let f t = result_of_judgement (root_of_tree t) in
      match (f t1, f t2) with
      | R_bool b1, R_bool b2 ->
          let b = fun_of_lop lop b1 b2 in
          Tree (J_bexp (store, bexp, b), rule_of_lop lop, [ t1; t2 ])
      | _ -> assert false)
  | B_comp (comp, a1, a2) -> (
      let t1 = derive_aexp a1 store in
      let t2 = derive_aexp a2 store in
      let f t = result_of_judgement (root_of_tree t) in
      match (f t1, f t2) with
      | R_int i1, R_int i2 ->
          let b = fun_of_comp comp i1 i2 in
          Tree (J_bexp (store, bexp, b), rule_of_comp comp, [ t1; t2 ])
      | _ -> assert false)

let rec update k v ret = function
  | [] -> (k, v) :: List.rev ret
  | (k', v') :: l ->
      if k = k' then List.rev_append ret ((k, v) :: l)
      else update k v ((k', v') :: ret) l

let update x i store = update x i [] store

let rec derive_com com store =
  match com with
  | C_skip -> Tree (J_changes (com, store, store), C_Skip, [])
  | C_assign (x, a) -> (
      let t = derive_aexp a store in
      match result_of_judgement (root_of_tree t) with
      | R_int i ->
          let store' = update x i store in
          Tree (J_changes (com, store, store'), C_Assign, [ t ])
      | _ -> assert false)
  | C_seq (c1, c2) -> (
      let t1 = derive_com c1 store in
      match result_of_judgement (root_of_tree t1) with
      | R_store store' -> (
          let t2 = derive_com c2 store' in
          match result_of_judgement (root_of_tree t2) with
          | R_store store'' ->
              Tree (J_changes (com, store, store''), C_Seq, [ t1; t2 ])
          | _ -> assert false)
      | _ -> assert false)
  | C_if (b, c1, c2) -> (
      let t1 = derive_bexp b store in
      match result_of_judgement (root_of_tree t1) with
      | R_bool b -> (
          let t2 = derive_com (if b then c1 else c2) store in
          match result_of_judgement (root_of_tree t2) with
          | R_store store' ->
              Tree
                ( J_changes (com, store, store'),
                  (if b then C_IfT else C_IfF),
                  [ t1; t2 ] )
          | _ -> assert false)
      | _ -> assert false)
  | C_while (b, c) -> (
      let t1 = derive_bexp b store in
      match result_of_judgement (root_of_tree t1) with
      | R_bool true -> (
          let t2 = derive_com c store in
          match result_of_judgement (root_of_tree t2) with
          | R_store store' -> (
              let t3 = derive_com com store' in
              match result_of_judgement (root_of_tree t3) with
              | R_store store'' ->
                  Tree
                    (J_changes (com, store, store''), C_WhileT, [ t1; t2; t3 ])
              | _ -> assert false)
          | _ -> assert false)
      | R_bool false -> Tree (J_changes (com, store, store), C_WhileF, [ t1 ])
      | _ -> assert false)

let derive = function
  | J_aexp (store, a, _) -> derive_aexp a store
  | J_bexp (store, b, _) -> derive_bexp b store
  | J_changes (c, store, _) -> derive_com c store
