{
open Parser
exception Eof
}
rule token = parse
    | [' ']    { token lexbuf } 
    | ['\n' ]  { EOL }
    | 'S'      { S }
    | 'Z'      { Z }
    | "is"     { IS }
    | "plus"   { NAT_PLUS }
    | "times"  { NAT_TIMES }
    | "+"      { PLUS }
    | "*"      { TIMES }
    | '('      { LPAREN }
    | ')'      { RPAREN }
    | "--->" { ONE_STEP }
    | "-*->" { MULTI_STEP }
    | "-d->" { DET_STEP }
    | eof      { raise Eof }
