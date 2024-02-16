let rec all_eq = fun la -> fun lb -> match la, lb with
| ([], []) -> true
| ([], _) -> false
| (_, []) -> false
| (x :: xs, y :: ys) -> x = y && all_eq ys xs
in
  all_eq [1; 1] [1; 1], all_eq [1] []
