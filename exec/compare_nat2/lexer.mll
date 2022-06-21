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
    | "less" { LESS }
    | "than" { THAN }
    | '('            { LPAREN }
    | ')'            { RPAREN }
    | eof            { raise Eof }
