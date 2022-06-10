module System = struct
  type op = Plus | Minus | Times | Lt
  type var = string

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

  type dbvalue =
    | DBV_int of int
    | DBV_bool of bool
    | DBV_fun of env * dbexp
    | DBV_letrec of env * dbexp

  and env = dbvalue list

  type judgement =
    | J_evalto of env * dbexp * dbvalue
    | J_is of (op * dbvalue * dbvalue) * dbvalue

  type rule =
    | E_Int
    | E_Bool
    | E_IfT
    | E_IfF
    | E_Plus
    | E_Minus
    | E_Times
    | E_Lt
    | E_Var
    | E_Let
    | E_Fun
    | E_App
    | E_LetRec
    | E_AppRec
    | B_Plus
    | B_Minus
    | B_Times
    | B_Lt

  let result_of_judgement = function J_evalto (_, _, v) | J_is (_, v) -> v

  let rule_of_op = function
    | Plus -> E_Plus
    | Minus -> E_Minus
    | Times -> E_Times
    | Lt -> E_Lt

  module String = struct
    let of_op = function Plus -> "+" | Minus -> "-" | Times -> "*" | Lt -> "<"

    let of_meta_op = function
      | Plus -> "plus"
      | Minus -> "minus"
      | Times -> "times"
      | Lt -> "less than"

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

    let rec of_dbvalue = function
      | DBV_int i -> string_of_int i
      | DBV_bool b -> string_of_bool b
      | DBV_fun (env, d) -> "(" ^ of_env env ^ ")[fun . -> " ^ of_dbexp d ^ "]"
      | DBV_letrec (env, d) ->
          "(" ^ of_env env ^ ")[rec . = fun . -> " ^ of_dbexp d ^ "]"

    and of_env = function
      | [] -> ""
      | [v] -> of_dbvalue v
      | v :: env -> of_env env ^ ", " ^ of_dbvalue v

    let of_judgement = function
      | J_evalto (env, d, v) ->
          of_env env ^ " |- " ^ of_dbexp d ^ " evalto " ^ of_dbvalue v
      | J_is ((op, v1, v2), v3) ->
          of_dbvalue v1 ^ " " ^ of_meta_op op ^ " " ^ of_dbvalue v2 ^ " is "
          ^ of_dbvalue v3

    let of_rule = function
      | E_Int -> "E-Int"
      | E_Bool -> "E-Bool"
      | E_IfT -> "E-IfT"
      | E_IfF -> "E-IfF"
      | E_Plus -> "E-Plus"
      | E_Minus -> "E-Minus"
      | E_Times -> "E-Times"
      | E_Lt -> "E-Lt"
      | E_Var -> "E-Var"
      | E_Let -> "E-Let"
      | E_Fun -> "E-Fun"
      | E_App -> "E-App"
      | E_LetRec -> "E-LetRec"
      | E_AppRec -> "E-AppRec"
      | B_Plus -> "B-Plus"
      | B_Minus -> "B-Minus"
      | B_Times -> "B-Times"
      | B_Lt -> "B-Lt"
  end

  let string_of_judgement = String.of_judgement
  let string_of_rule = String.of_rule
end

module Tree = Core.Derivation_tree.Make (System)
open System
open Tree

let rec derive_exp e env =
  match e with
  | DB_int i -> Tree (J_evalto (env, e, DBV_int i), E_Int, [])
  | DB_bool b -> Tree (J_evalto (env, e, DBV_bool b), E_Bool, [])
  | DB_var n ->
      let w = List.nth env (n - 1) in
      Tree (J_evalto (env, e, w), E_Var, [])
  | DB_if (e1, e2, e3) -> (
      let t1 = derive_exp e1 env in
      let v1 = result_of_judgement (root_of_tree t1) in
      match v1 with
      | DBV_bool true ->
          let t2 = derive_exp e2 env in
          let v = result_of_judgement (root_of_tree t2) in
          Tree (J_evalto (env, e, v), E_IfT, [t1; t2])
      | DBV_bool false ->
          let t3 = derive_exp e3 env in
          let v = result_of_judgement (root_of_tree t3) in
          Tree (J_evalto (env, e, v), E_IfF, [t1; t3])
      | _ -> raise (Invalid_argument "Conditional must be bool.") )
  | DB_BinOp (op, e1, e2) ->
      let t1 = derive_exp e1 env in
      let v1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 env in
      let v2 = result_of_judgement (root_of_tree t2) in
      let t3 = derive_nat_bin_op (op, v1, v2) in
      let v3 = result_of_judgement (root_of_tree t3) in
      Tree (J_evalto (env, e, v3), rule_of_op op, [t1; t2; t3])
  | DB_let (e1, e2) ->
      let t1 = derive_exp e1 env in
      let v1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 (v1 :: env) in
      let v = result_of_judgement (root_of_tree t2) in
      Tree (J_evalto (env, e, v), E_Let, [t1; t2])
  | DB_fun e' -> Tree (J_evalto (env, e, DBV_fun (env, e')), E_Fun, [])
  | DB_app (e1, e2) -> (
      let t1 = derive_exp e1 env in
      let v1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 env in
      let v2 = result_of_judgement (root_of_tree t2) in
      match v1 with
      | DBV_fun (env2, e0) ->
          let t3 = derive_exp e0 (v2 :: env2) in
          let v = result_of_judgement (root_of_tree t3) in
          Tree (J_evalto (env, e, v), E_App, [t1; t2; t3])
      | DBV_letrec (env2, e0) ->
          let t3 = derive_exp e0 (v2 :: v1 :: env2) in
          let v = result_of_judgement (root_of_tree t3) in
          Tree (J_evalto (env, e, v), E_AppRec, [t1; t2; t3])
      | _ -> assert false )
  | DB_letrec (e1, e2) ->
      let env2 = DBV_letrec (env, e1) :: env in
      let t = derive_exp e2 env2 in
      let v = result_of_judgement (root_of_tree t) in
      Tree (J_evalto (env, e, v), E_LetRec, [t])

and derive_nat_bin_op nat_bin_op =
  match nat_bin_op with
  | Plus, DBV_int i1, DBV_int i2 ->
      Tree (J_is (nat_bin_op, DBV_int (i1 + i2)), B_Plus, [])
  | Minus, DBV_int i1, DBV_int i2 ->
      Tree (J_is (nat_bin_op, DBV_int (i1 - i2)), B_Minus, [])
  | Times, DBV_int i1, DBV_int i2 ->
      Tree (J_is (nat_bin_op, DBV_int (i1 * i2)), B_Times, [])
  | Lt, DBV_int i1, DBV_int i2 ->
      Tree (J_is (nat_bin_op, DBV_bool (i1 < i2)), B_Lt, [])
  | _ -> raise (Invalid_argument "No evaluation rule can be applied.")

let derive = function
  | J_evalto (env, d, _) -> derive_exp d env
  | J_is (t, _) -> derive_nat_bin_op t
