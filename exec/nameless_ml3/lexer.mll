{
open Parser
exception Eof
}
rule token = parse
    | "\n\n" | [ ';' ]  { EOL }
    | [' ' '\n' ]    { token lexbuf } 
    | "-"? ['0'-'9']+ { VINT (int_of_string (Lexing.lexeme lexbuf)) }
    | "true" { VBOOL true }
    | "false" { VBOOL false}
    | "+"   { PLUS }
    | "-" { MINUS }
    | "*" { TIMES }
    | "<" { LT }
    | "if" { IF }
    | "then" { THEN}
    | "else" {ELSE }
    | '('      { LPAREN }
    | ')'      { RPAREN }
    | "let" { LET }
    | "=" { EQ }
    | "in" { IN }
    | "|-" { PROVE }
    | "," { COMMA }
    | "fun" { FUN }
    | "->" { ARROW }
    | "rec" { REC }
    | "#" { SHARP }
    | "==>" { COMPILE }
    | "." { DOT}
    | ['a'-'z']+ { VAR (Lexing.lexeme lexbuf) }
    | eof      { raise Eof }
