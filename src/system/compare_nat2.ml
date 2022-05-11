module System = struct
  type nat = Z | S of nat
  type judgement = J_less of nat * nat
  type rule = L_Zero | L_SuccSucc

  module String = struct
    let rec of_nat = function Z -> "Z" | S n -> "S(" ^ of_nat n ^ ")"

    let of_judgement = function
      | J_less (n1, n2) -> of_nat n1 ^ " is less than " ^ of_nat n2

    let of_rule = function L_Zero -> "L-Zero" | L_SuccSucc -> "L-SuccSucc"
  end

  let string_of_judgement = String.of_judgement
  let string_of_rule = String.of_rule
end

module Tree = Core.Derivation_tree.Make (System)
open System
open Tree

let rec derive judgement =
  match judgement with
  | J_less (Z, S _) -> Tree (judgement, L_Zero, [])
  | J_less (S n1, S n2) ->
      let t = derive (J_less (n1, n2)) in
      Tree (judgement, L_SuccSucc, [t])
  | _ ->
      raise
        (Invalid_argument (string_of_judgement judgement ^ " does not hold."))
