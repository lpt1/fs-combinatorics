namespace FSCombinatorics

module Permutations =

  ///Python's itertools.product() in F#
  ///permute ['A'; 'B'] 2
  ///> [['A'; 'A']; ['B'; 'A']; ['A'; 'B']; ['B'; 'B']]
  let permute (arg: 'a list) (times: int) =
    let pools = [ for n in 1..times -> arg ]
    
    let rec loop lls acc = 
      match lls with
      | [] -> acc    
      | h :: t -> loop t [ for x in h do 
                           for y in acc do 
                           yield y @ [x] ]
    loop pools [[]]



module Compositions =
  let private mapcons x = List.map (fun xs -> x :: xs)

  /// Calculate the integer compositions of argument n
  ///TODO make tail recursive
  let rec intCompositions = function
      | 1 -> [[1]]
      | n -> [n] :: [ for x in 1..(n - 1) do 
                      yield! (mapcons (n - x)) (intCompositions x) ]
