{
open Parser
exception Eof
}
rule token = parse
    | "\n\n" | [ ';' ]  { EOL }
    | [' ' '\n' ]    { token lexbuf } 
    | 'S' { S }
    | 'Z' { Z }
    | "is" { IS }
    | "plus"           { PLUS }
    | "times"            { TIMES }
    | '('            { LPAREN }
    | ')'            { RPAREN }
    | eof            { raise Eof }
