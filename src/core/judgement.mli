module type Judgement =
  sig
    type judgement
    type rule
    val string_of_judgement : judgement -> string
    val string_of_rule : rule -> string
  end
