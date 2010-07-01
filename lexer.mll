{
  open Misc
  open Parser

  exception Lexical_error
    
  let reserv_words = [
  ]
}

let ident_top = ['a'-'z' '_']
let ident_bdy = ['a'-'z' '_' '\'']
let blank = [' ' '\009' '\012' '\n']
rule main = parse
  blank+ { main lexbuf }
| '\\'   { Parser.BACKSLA }
| ':'    { Parser.COLON }
| "->"   { Parser.RARROW }
| '+'    { Parser.PLUS }
| '*'    { Parser.ASTER }
| '/'    { Parser.SLASH }
| '('    { Parser.LPAREN }
| ')'    { Parser.RPAREN }
| "\\\\" { Parser.BACKSLA2 }
| ";;"   { Parser.SEMICOLON2 }
| "-"? [ '0'-'9' ]+ { Parser.INTLIT (int_of_string $ Lexing.lexeme lexbuf) }
| ident_top ident_bdy*
      { let s = Lexing.lexeme lexbuf in
        try List.assoc s reserv_words with
          Not_found -> IDENT s
      }
| eof    { exit 0 }
