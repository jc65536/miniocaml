let(true, var(all_eq),              
  fun(var(la),
    fun(var(lb),
      match(tuple([var(la), var(lb)]), [
        branch(tuple([list([]), list([])]), bool(true)),
        branch(tuple([list([]), any]), bool(false)),
        branch(tuple([any, list([])]), bool(false)),
        branch(tuple([cons(var(x), var(xs)), cons(var(y), var(ys))]),
          bop(bop(var(x), "=", var(y)), "&&",
          app(var(all_eq), [var(ys), var(xs)])))]))),
  tuple([
    app(var(all_eq), [list([int(1), int(1)]), list([int(1), int(1)])]),
    app(var(all_eq), [list([int(1)]), list([])])]))
