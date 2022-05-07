type token = Line of string | Indent | Unindent

let indent_width = 2
let make_indent level = String.make (level * indent_width) ' '

let rec string_list_of_token_list indent_level = function
  | [] -> []
  | Line s :: token_list ->
      (make_indent indent_level ^ s)
      :: string_list_of_token_list indent_level token_list
  | Indent :: token_list ->
      string_list_of_token_list (indent_level + 1) token_list
  | Unindent :: token_list ->
      string_list_of_token_list (indent_level - 1) token_list

let string_of_token_list token_list =
  String.concat "\n" (string_list_of_token_list 0 token_list)
