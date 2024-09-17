let(false, var(not),                
  fun(var(b),
    match(var(b), [
      branch(bool(true), bool(false)),
      branch(bool(false), bool(true))])),
  let(true, var(short_fold),
    fun(var(f),
      fun(var(init),
        fun(var(list),
          match(var(list), [
            branch(cons(var(x), var(xs)),
              let(false, tuple([var(cont), var(acc)]),
                app(var(f), [var(init), var(x)]),
                if(var(cont),
                  app(var(short_fold), [var(f), var(acc), var(xs)]),
                  var(acc)))),
            branch(any, var(init))])))),
    let(false, var(prod),
      app(var(short_fold),
        [
        fun(var(acc),
          fun(var(x),
            tuple([
              app(var(not), [bop(var(x), "=", int(0))]),
              bop(var(acc), "*", var(x))]))),
        int(1)]),
      app(var(prod), [list([int(3), int(4), int(5), int(0), int(6), int(7)])]))
    ))
