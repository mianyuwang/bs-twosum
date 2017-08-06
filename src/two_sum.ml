
let find_index_of nums v =
  let rec impl nums v =
    match nums with
    | [] -> failwith "Not found"
    | hd :: tl ->
      if (hd = v) then 0
      else 1 + (impl tl v) in
  try impl nums v with
  | Failure _ -> -1 

let rec two_sum_by_list nums target =
  match nums with
  | []       -> failwith "Not found"
  | hd :: tl ->
    match find_index_of tl (target - hd) with
    | fi when fi >= 0 ->
      (0, fi + 1)
    | _ -> 
      let (i, j) = two_sum_by_list tl target in
      (i + 1, j + 1)

let twoSum nums target =
  two_sum_by_list (Array.to_list nums) target

let () =
  let test = [| 2; 7; 11; 15|] in
  let (i, j) = twoSum test 18 in
  Js.log ("Two numbers make the sum are: "
         ^ (string_of_int i)
         ^ ", "
         ^ (string_of_int j))
         