open Misc

exception Exit
  
let err s = print_endline s 
let p_msg s = print_endline s
  
let rec read_eval_print prompt fun_lexbuf env tenv types err =
  print_string prompt;
  flush stdout;
  try 
    match Parser.main Lexer.main @< Lexing.from_function fun_lexbuf with
      Syntax.EOF -> (env, tenv, types)
    | Syntax.Prog decl -> begin
        let typ = Typing.typing tenv types decl in
        let newenv, id ,v = Eval.eval env decl in
        let newenv = Env.extend newenv id v in
        let newtenv = Env.extend tenv id typ in
        print_string @< "val "^id^" : ";
        Typing.pp_typ typ;
        print_string " = ";
        Eval.pp_val v;
        print_newline ();
        read_eval_print prompt fun_lexbuf newenv newtenv types err
      end
  with
    e -> let f s = err s; read_eval_print in
    (match e with
       Parsing.Parse_error   -> f "Syntax error" prompt fun_lexbuf env tenv types err 
     | Lexer.Lexical_error   -> f "Lexical error" prompt fun_lexbuf env tenv types err
     | Eval.Eval_error s     -> f s prompt fun_lexbuf env tenv types err
     | Typing.Typing_error s -> f s prompt fun_lexbuf env tenv types err
     | _ -> raise e)

let refill_buffer ch =
  let rec fill_buff dst idx len acc =
    if len = acc then acc
    else begin
      try 
        let c = input_char ch in
        dst.[idx] <- c;
        if c = '\n' then acc+1
        else fill_buff dst (idx+1) len (acc+1)
      with
        End_of_file -> acc
    end
  in
  let body buf len = fill_buff buf 0 len 0 in
  body

let init_env = Env.extend (Env.extend Env.empty "i" @< Eval.IntV 1) "ii" @< Eval.IntV 2
let init_tenv = Env.extend (Env.extend Env.empty "i" Syntax.IntT) "ii" Syntax.IntT
let init_types = Env.extend Env.empty "int" Syntax.IntT
    
let main () =
  let files = ref [] in
  let interact = ref None in
  let _ =
    Arg.parse ["-i", Arg.Unit (fun _ -> interact := Some true), "interact with interpretor"]
      (fun f -> files := f::!files; if !interact = None then interact := Some false else ())
      "Usage: Interpretor for System F"
  in
  let interact = match !interact with None -> true | Some b -> b in
  let ferr s = err s; raise Exit in
  let files =
    List.fold_left
      (fun xs file ->
         ((try open_in file with Sys_error _ -> err @< Printf.sprintf "%s: No such file." file; raise Exit), ferr, Some file, "")::xs)
      [] !files
  in
  let ins = List.append files @< if interact then [(stdin, (fun s -> err s; read_eval_print), None, "> ")] else [] in
  List.fold_left
    (fun (env, tenv, types) (ichann, err, file, prompt) ->
       (* print file name *)
       (match file with
          None -> ()
        | Some file -> p_msg @< Printf.sprintf "will load: %s" file);
       read_eval_print prompt (refill_buffer ichann) env tenv types err)
    (init_env, init_tenv, init_types)
    ins

let _ = try ignore @< main () with Exit -> ();
