let rec (a, b) = (fun x -> b (x - 1)), (fun x -> a (x + 1))

let () = (1, 2)
