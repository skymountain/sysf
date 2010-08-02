open Misc

let err s = print_endline s 
  
let rec read_eval_print prompt ch env tenv types =
    print_string prompt;
    flush stdout;
    try 
      let decl = Parser.main Lexer.main @< Lexing.from_channel ch in
      let typ = Typing.typing tenv types decl in
      let newenv, id ,v = Eval.eval env decl in
      let newenv = Env.extend newenv id v in
      print_string @< "val "^id^" : ";
      Typing.pp_typ typ;
      print_string " = ";
      Eval.pp_val v;
      print_newline ();
      read_eval_print prompt ch newenv tenv types
    with
      e -> let f s = err s; read_eval_print prompt ch env tenv types in
      (match e with
         Parsing.Parse_error   -> f "Syntax error"
       | Lexer.Lexical_error   -> f "Lexical error"
       | Eval.Eval_error s     -> f s
       | Typing.Typing_error s -> f s
       | _ -> raise e)
        
let init_env = Env.extend (Env.extend Env.empty "i" @< Eval.IntV 1) "ii" @< Eval.IntV 2
let init_tenv = Env.extend (Env.extend Env.empty "i" Syntax.IntT) "ii" Syntax.IntT
let init_types = Env.extend Env.empty "int" Syntax.IntT
  
let _ = read_eval_print "> " stdin init_env init_tenv init_types
