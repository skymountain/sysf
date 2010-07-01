open Misc

let err s = print_endline s 
  
let rec read_eval_print prompt ch env =
    print_string prompt;
    flush stdout;
    try 
      let decl = Parser.main Lexer.main $ Lexing.from_channel ch in
      let newenv, id ,v = Eval.eval env decl in
      let newenv = Env.extend newenv id v in
      print_string $ "val "^id^" = ";
      Eval.pp_val v;
      print_newline ();
      read_eval_print prompt ch newenv
    with
      Parsing.Parse_error  -> err "Syntax error"; read_eval_print prompt ch env
    | Lexer.Lexical_error  -> err "Lexical error"; read_eval_print prompt ch env
    | Eval.Eval_error s -> err s; read_eval_print prompt ch env
        
let init_env = Env.extend $ (Env.extend Env.empty "i" $ Eval.IntV 1) $ "ii" $ Eval.IntV 2
  

let _ = read_eval_print "> " stdin init_env
