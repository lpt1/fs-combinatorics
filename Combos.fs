module Combos

(*
My F# functional version of Python's itertools.repeat()

repeat [0,1] 2

>>> [x+[y] for x in [[]] for y in (0,1)]
[[0], [1]]
>>> [x+[y] for x in [[0], [1]] for y in (0,1)]
[[0, 0], [0, 1], [1, 0], [1, 1]]

*)

let repeat (arg: 'a list) (repeat: int) =
  let pools = [ for n in 1..repeat -> arg ] // pools=[[0,1]; [0,1]]
  
  let rec loop lls acc = 
    match lls with
    | [] -> acc    
    | h :: t -> loop t [for x in h do for y in acc do yield y @ [x]]

  loop pools [[]]
