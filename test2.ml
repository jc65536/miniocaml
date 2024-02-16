let rec all_eq = fun l1 -> fun l2 -> match l1, l2 with
| ([], []) -> true
| ([], _) -> false
| (_, []) -> false
| (x :: xs, y :: ys) -> x = y && all_eq ys xs
in
  all_eq [1; 1] [1; 1], all_eq [1] [], all_eq [1; 1] [true; true]
