let rec short_fold = fun f -> fun init -> fun list -> match list with
  | x :: xs -> let ( cont , acc ) = f init x in
    if cont then short_fold f acc xs else acc
  | _ -> init
in
  let prod = short_fold ( fun acc x -> ( not ( x = 0 ) , acc * x ) ) 1
  in
    prod [ 3 ; 4 ; 5 ; 0 ; 6 ; 7 ]
