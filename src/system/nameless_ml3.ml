module System = struct
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

  type dbexp =
    | DB_int of int
    | DB_bool of bool
    | DB_var of int
    | DB_BinOp of op * dbexp * dbexp
    | DB_if of dbexp * dbexp * dbexp
    | DB_let of dbexp * dbexp
    | DB_fun of dbexp
    | DB_app of dbexp * dbexp
    | DB_letrec of dbexp * dbexp

  and env = var list

  type judgement = J_compile of env * exp * dbexp

  type rule =
    | Tr_Int
    | Tr_Bool
    | Tr_If
    | Tr_Plus
    | Tr_Minus
    | Tr_Times
    | Tr_Lt
    | Tr_Var1
    | Tr_Var2
    | Tr_Let
    | Tr_Fun
    | Tr_App
    | Tr_LetRec

  let result_of_judgement = function J_compile (_, _, d) -> d

  let rule_of_op = function
    | Plus -> Tr_Plus
    | Minus -> Tr_Minus
    | Times -> Tr_Times
    | Lt -> Tr_Lt

  module String = struct
    let of_op = function Plus -> "+" | Minus -> "-" | Times -> "*" | Lt -> "<"

    let of_meta_op = function
      | Plus -> "plus"
      | Minus -> "minus"
      | Times -> "times"
      | Lt -> "less than"

    let rec of_exp = function
      | E_int i -> string_of_int i
      | E_bool b -> string_of_bool b
      | E_var x -> x
      | BinOp (((Plus | Minus) as op), e1, e2) ->
          let s1 =
            match e1 with E_if _ -> "(" ^ of_exp e1 ^ ")" | _ -> of_exp e1 in
          let s2 =
            match e2 with
            | BinOp ((Plus | Minus), _, _) | E_if _ -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2 in
          s1 ^ " " ^ of_op op ^ " " ^ s2
      | BinOp (Times, e1, e2) ->
          let s1 =
            match e1 with
            | BinOp ((Plus | Minus), _, _) | E_if _ -> "(" ^ of_exp e1 ^ ")"
            | _ -> of_exp e1 in
          let s2 =
            match e2 with
            | BinOp (_, _, _) | E_if _ -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2 in
          s1 ^ " " ^ of_op Times ^ " " ^ s2
      | BinOp (Lt, e1, e2) ->
          let s1 =
            match e1 with E_if _ -> "(" ^ of_exp e1 ^ ")" | _ -> of_exp e1 in
          let s2 =
            match e2 with E_if _ -> "(" ^ of_exp e2 ^ ")" | _ -> of_exp e2 in
          s1 ^ " " ^ of_op Lt ^ " " ^ s2
      | E_if (e1, e2, e3) ->
          "if " ^ of_exp e1 ^ " then " ^ of_exp e2 ^ " else " ^ of_exp e3
      | E_let (x, e1, e2) -> "let " ^ x ^ " = " ^ of_exp e1 ^ " in " ^ of_exp e2
      | E_fun (x, e) -> "fun " ^ x ^ " -> " ^ of_exp e
      | E_app (e1, e2) ->
          let wrap = function
            | (E_var _ | E_int _ | E_bool _) as e -> of_exp e
            | e -> "(" ^ of_exp e ^ ")" in
          wrap e1 ^ " " ^ wrap e2
      | E_letrec (f, x, e1, e2) ->
          "let rec " ^ f ^ " = fun " ^ x ^ " -> " ^ of_exp e1 ^ " in "
          ^ of_exp e2

    let rec of_dbexp = function
      | DB_int i -> string_of_int i
      | DB_bool b -> string_of_bool b
      | DB_var i -> "#" ^ string_of_int i
      | DB_BinOp (((Plus | Minus) as op), d1, d2) ->
          let s1 =
            match d1 with
            | DB_if _ -> "(" ^ of_dbexp d1 ^ ")"
            | _ -> of_dbexp d1 in
          let s2 =
            match d2 with
            | DB_BinOp ((Plus | Minus), _, _) | DB_if _ ->
                "(" ^ of_dbexp d2 ^ ")"
            | _ -> of_dbexp d2 in
          s1 ^ " " ^ of_op op ^ " " ^ s2
      | DB_BinOp (Times, d1, d2) ->
          let s1 =
            match d1 with
            | DB_BinOp ((Plus | Minus), _, _) | DB_if _ ->
                "(" ^ of_dbexp d1 ^ ")"
            | _ -> of_dbexp d1 in
          let s2 =
            match d2 with
            | DB_BinOp (_, _, _) | DB_if _ -> "(" ^ of_dbexp d2 ^ ")"
            | _ -> of_dbexp d2 in
          s1 ^ " " ^ of_op Times ^ " " ^ s2
      | DB_BinOp (Lt, d1, d2) ->
          let s1 =
            match d1 with
            | DB_if _ -> "(" ^ of_dbexp d1 ^ ")"
            | _ -> of_dbexp d1 in
          let s2 =
            match d2 with
            | DB_if _ -> "(" ^ of_dbexp d2 ^ ")"
            | _ -> of_dbexp d2 in
          s1 ^ " " ^ of_op Lt ^ " " ^ s2
      | DB_if (d1, d2, d3) ->
          "if " ^ of_dbexp d1 ^ " then " ^ of_dbexp d2 ^ " else " ^ of_dbexp d3
      | DB_let (d1, d2) -> "let . = " ^ of_dbexp d1 ^ " in " ^ of_dbexp d2
      | DB_fun d -> "fun . -> " ^ of_dbexp d
      | DB_app (d1, d2) ->
          let wrap = function
            | (DB_var _ | DB_int _ | DB_bool _) as d -> of_dbexp d
            | d -> "(" ^ of_dbexp d ^ ")" in
          wrap d1 ^ " " ^ wrap d2
      | DB_letrec (d1, d2) ->
          "let rec . = fun . -> " ^ of_dbexp d1 ^ " in " ^ of_dbexp d2

    and of_env = function
      | [] -> ""
      | [x] -> x
      | x :: env -> of_env env ^ ", " ^ x

    let of_judgement = function
      | J_compile (env, e, d) ->
          of_env env ^ " |- " ^ of_exp e ^ " ==> " ^ of_dbexp d

    let of_rule = function
      | Tr_Int -> "Tr-Int"
      | Tr_Bool -> "Tr-Bool"
      | Tr_If -> "Tr-If"
      | Tr_Plus -> "Tr-Plus"
      | Tr_Minus -> "Tr-Minus"
      | Tr_Times -> "Tr-Times"
      | Tr_Lt -> "Tr-Lt"
      | Tr_Var1 -> "Tr-Var1"
      | Tr_Var2 -> "Tr-Var2"
      | Tr_Let -> "Tr-Let"
      | Tr_Fun -> "Tr-Fun"
      | Tr_App -> "Tr-App"
      | Tr_LetRec -> "Tr-LetRec"
  end

  let string_of_judgement = String.of_judgement
  let string_of_rule = String.of_rule
end

module Tree = Core.Derivation_tree.Make (System)
open System
open Tree

let rec derive_exp e env =
  match e with
  | E_int i -> Tree (J_compile (env, e, DB_int i), Tr_Int, [])
  | E_bool b -> Tree (J_compile (env, e, DB_bool b), Tr_Bool, [])
  | E_if (e1, e2, e3) ->
      let t1 = derive_exp e1 env in
      let d1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 env in
      let d2 = result_of_judgement (root_of_tree t2) in
      let t3 = derive_exp e3 env in
      let d3 = result_of_judgement (root_of_tree t3) in
      Tree (J_compile (env, e, DB_if (d1, d2, d3)), Tr_If, [t1; t2; t3])
  | BinOp (op, e1, e2) ->
      let t1 = derive_exp e1 env in
      let d1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 env in
      let d2 = result_of_judgement (root_of_tree t2) in
      Tree (J_compile (env, e, DB_BinOp (op, d1, d2)), rule_of_op op, [t1; t2])
  | E_var x -> (
    match env with
    | x' :: env' -> (
        if x = x' then Tree (J_compile (env, e, DB_var 1), Tr_Var1, [])
        else
          let t = derive_exp e env' in
          let d = result_of_judgement (root_of_tree t) in
          match d with
          | DB_var n -> Tree (J_compile (env, e, DB_var (n + 1)), Tr_Var2, [t])
          | _ -> assert false )
    | _ -> assert false )
  | E_let (x, e1, e2) ->
      let t1 = derive_exp e1 env in
      let d1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 (x :: env) in
      let d2 = result_of_judgement (root_of_tree t2) in
      Tree (J_compile (env, e, DB_let (d1, d2)), Tr_Let, [t1; t2])
  | E_fun (x, e') ->
      let t = derive_exp e' (x :: env) in
      let d = result_of_judgement (root_of_tree t) in
      Tree (J_compile (env, e, DB_fun d), Tr_Fun, [t])
  | E_app (e1, e2) ->
      let t1 = derive_exp e1 env in
      let d1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 env in
      let d2 = result_of_judgement (root_of_tree t2) in
      Tree (J_compile (env, e, DB_app (d1, d2)), Tr_App, [t1; t2])
  | E_letrec (x, y, e1, e2) ->
      let env2 = x :: env in
      let t1 = derive_exp e1 (y :: env2) in
      let d1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 env2 in
      let d2 = result_of_judgement (root_of_tree t2) in
      Tree (J_compile (env, e, DB_letrec (d1, d2)), Tr_LetRec, [t1; t2])

let derive = function J_compile (env, e, _) -> derive_exp e env
