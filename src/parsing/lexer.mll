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
    | "plus"           { PLUS }
    | "times"            { TIMES }
    | '('            { LPAREN }
    | ')'            { RPAREN }
    | eof            { raise Eof }
