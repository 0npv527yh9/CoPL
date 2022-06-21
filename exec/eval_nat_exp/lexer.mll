{
open Parser
exception Eof
}
rule token = parse
    | "\n\n" | [ ';' ]  { EOL }
    | [' ' '\n' ]    { token lexbuf } 
    | 'S'      { S }
    | 'Z'      { Z }
    | "is"     { IS }
    | "plus"   { NAT_PLUS }
    | "times"  { NAT_TIMES }
    | "+"      { PLUS }
    | "*"      { TIMES }
    | '('      { LPAREN }
    | ')'      { RPAREN }
    | "evalto" { EVALTO }
    | eof      { raise Eof }
