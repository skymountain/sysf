open Misc
  
type 'a t = 'a list

let empty = []
  
let extend env key v = (key, v)::env
let extendl env l = l @ env
let lookup env key = OptionList.assoc key env

let mem env key = match lookup env key with
    None -> false
  | _    -> true
  
let rec pps_env = function
    [] -> "[]"
  | (key,v)::xs -> Printf.sprintf "key:%s, %s" key @< pps_env xs
  
