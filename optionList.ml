open Misc
open Option

let length x = suc $ List.length x
let hd x =
  try suc $ List.hd x with
    e -> failure "hd" e
let tl x =
  try suc $ List.tl x with
    e -> failure "tl" e
let nth x =
  try suc $ List.nth x with
    Failure "nth" | Invalid_argument "List.nth" -> None
let rev x = suc $ List.rev x
let append x y = suc $ List.append x y
let rev_append x y = suc $ List.rev_append x y
let concat x = suc $ List.concat x
let flatten x = suc $ List.flatten x
let iter x y = suc $ List.iter x y
let map x y = suc $ List.map x y
let rev_map x y = suc $ List.rev_map x y
let fold_left x y = suc $ List.fold_left x y
let fold_right x y = suc $ List.fold_right x y
  
let iter2 x y z =
  try suc $ List.iter2 x y z with
    e -> invalid_argument "List.iter2" e
let map2 x y z =
  try suc $ List.map2 x y z with
    e -> invalid_argument "List.map2" e
let rev_map2 x y z =
  try suc $ List.rev_map2 x y z with
    e -> invalid_argument "List.rev_map2" e
let fold_left2 a b c d =
  try suc $ List.fold_left2 a b c d with
    e -> invalid_argument "List.fold_left2" e
let fold_right2 a b c d =
  try suc $ List.fold_right2 a b c d with
    e -> invalid_argument "List.fold_right2" e

let for_all x y = suc $ List.for_all x y
let exists x y = suc $ List.exists x y
let for_all2 x y z =
  try suc $ List.for_all2 x y z with
    e -> invalid_argument "List.for_all2" e
let exists2 x y z =
  try suc $ List.exists2 x y z with
    e -> invalid_argument "List.exists2" e
let mem x y = suc $ List.mem x y
let memq x y = suc $ List.memq x y
  
let find x y =
  try suc $ List.find x y with
    e -> not_found e
let filter x y = suc $ List.filter x y
let find_all x y = suc $ List.find_all x y
let partition x y = suc $ List.partition x y
  
let assoc x y =
  try suc $ List.assoc x y with
    e -> not_found e
let assq x y =
  try suc $ List.assq x y with
    e -> not_found e
let mem_assoc x y =
  try suc $ List.mem_assoc x y with
    e -> not_found e
let mem_assq x y =
  try suc $ List.mem_assq x y with
    e -> not_found e
let remove_assoc x y = suc $ List.remove_assoc x y
let remove_assq x y = suc $ List.remove_assq x y
  
let split x = suc $ List.split x
let combine x y =
  try suc $ List.combine x y with
    e -> invalid_argument "List.combine" e

let sort x y = suc $ List.sort x y
let stable_sort x y = suc $ List.stable_sort x y
let fast_sort x y = suc $ List.fast_sort x y
let merge x y = suc $ List.merge x y
