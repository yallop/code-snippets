(* A simple example lexer from the OCaml documentation
   http://caml.inria.fr/pub/docs/manual-ocaml/manual026.html#toc110
 *)
{
type token = INT of int | PLUS | EOL | MINUS | TIMES | DIV | LPAREN | RPAREN 

let show_token = function
  | INT i  -> "INT " ^ string_of_int i
  | PLUS   -> "PLUS"
  | EOL    -> "EOL"
  | MINUS  -> "MINUS"
  | TIMES  -> "TIMES"
  | DIV    -> "DIV"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"

exception Eof
}
rule lex = parse
    [' ' '\t']     { lex lexbuf }     (* skip blanks *)
  | ['\n' ]        { EOL }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | eof            { raise Eof }
