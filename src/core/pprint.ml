type token = Line of string | Indent | Unindent

let indent_width = 2
let make_indent level = String.make (level * indent_width) ' '
let last_of_string s = s.[String.length s - 1]

let rec string_list_of_token_list indent_level = function
  | [] -> []
  | Line s :: token_list ->
      (make_indent indent_level ^ s)
      :: string_list_of_token_list indent_level token_list
  | Indent :: token_list ->
      string_list_of_token_list (indent_level + 1) token_list
  | Unindent :: token_list ->
      string_list_of_token_list (indent_level - 1) token_list

let rec simplify rev_string_list = function
  | [] -> List.rev_append rev_string_list []
  | s1 :: s2 :: s when last_of_string s1 = '{' && last_of_string s2 = '}' ->
      simplify rev_string_list ((s1 ^ "}") :: s)
  | s1 :: s2 :: s when last_of_string s1 = '}' && last_of_string s2 = ';' ->
      simplify rev_string_list ((s1 ^ ";") :: s)
  | s :: string_list -> simplify (s :: rev_string_list) string_list

let string_of_token_list token_list =
  let string_list = string_list_of_token_list 0 token_list in
  let string_list = simplify [] string_list in
  String.concat "\n" string_list
