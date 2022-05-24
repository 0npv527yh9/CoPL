{
open Parser
exception Eof
}
rule token = parse
    | [' ']     { token lexbuf } 
    | ['\n' ]        { EOL }
    | 'S' { S }
    | 'Z' { Z }
    | "is" { IS }
    | "less" { LESS }
    | "than" { THAN }
    | '('            { LPAREN }
    | ')'            { RPAREN }
    | eof            { raise Eof }
