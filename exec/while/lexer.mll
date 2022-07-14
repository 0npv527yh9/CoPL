{
open Parser
exception Eof
}
rule token = parse
    | [' ' '\n' ]    { token lexbuf } 
    | "\n\n" | ";;"  { EOL }
    | "-"? ['0'-'9']+ { VINT (int_of_string (Lexing.lexeme lexbuf)) }
    | "true" { VBOOL true }
    | "false" { VBOOL false}
    | "+"   { PLUS }
    | "-" { MINUS }
    | "*" { TIMES }
    | "<" { LT }
    | "<=" { LE }
    | "if" { IF }
    | "then" { THEN}
    | "else" {ELSE }
    | '('      { LPAREN }
    | ')'      { RPAREN }
    | "=" { EQ } 
    | "," { COMMA }
    | "&&" { AND}
    | "||" { OR }
    | ";" { SEMI }
    | ":=" { ASSIGN }
    | "while" { WHILE }
    | "do" { DO }
    | "skip" { SKIP }
    | "changes" { CHANGES }
    | "to" { TO }
    | "!" { NOT }
    | ['a'-'z']['a'-'z' '0'-'9']* { VAR (Lexing.lexeme lexbuf) }
    | eof      { raise Eof }
