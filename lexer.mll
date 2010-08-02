{
  open Misc
  open Parser

  exception Lexical_error
  let reserv_words = [
  ]
}

let alphabet  = ['a'-'z']
let ident_top = ['a'-'z' '_']
let ident_bdy = ['a'-'z' '_' '\'']
let blank = [' ' '\009' '\012' '\n']
rule main = parse
  blank+ { main lexbuf }
| '\\'   { Parser.BACKSLA }
| ':'    { Parser.COLON }
| '+'    { Parser.PLUS }
| '*'    { Parser.ASTER }
| '/'    { Parser.SLASH }
| '('    { Parser.LPAREN }
| ')'    { Parser.RPAREN }
| '['    { Parser.LSQPAREN }
| ']'    { Parser.RSQPAREN }
| '.'    { Parser.DOT }
| "\\\\" { Parser.BACKSLA2 }
| ";;"   { Parser.SEMICOLON2 }
| "->"   { Parser.RARROW }
| "-"? [ '0'-'9' ]+ { Parser.INTLIT (int_of_string @< Lexing.lexeme lexbuf) }
| ident_top ident_bdy*
      { let s = Lexing.lexeme lexbuf in
        try List.assoc s reserv_words with
          Not_found -> IDENT s
      }
| '\'' alphabet+ { let s = Lexing.lexeme lexbuf in
                   Parser.APOSTIDENT (String.sub s 1 @< String.length s - 1) }
| eof    { exit 0 }
