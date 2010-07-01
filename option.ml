let suc x = Some x

let fail orig target =
  if orig = target then None
  else raise target
    
let failure s e = fail (Failure s) e
let not_found e = fail Not_found e
let invalid_argument s e = fail (Invalid_argument s) e
