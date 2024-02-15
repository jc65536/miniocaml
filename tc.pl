parse_tree(
o_let(true, o_var(short_fold),
  o_fun([o_var(f)],
    o_fun([o_var(init)],
      o_fun([o_var(list)],
        o_match(o_var(list), [
          o_branch(o_cons(o_var(x), o_var(xs)),
            o_let(false, o_tup([o_var(cont), o_var(acc)]),
              o_app(o_var(f), o_var(init), o_var(x)),
              o_if(o_var(cont),
              o_app(o_var(short_fold), o_var(f), o_var(acc), o_var(xs)),
              o_var(acc)))),
          o_branch(o_wild, o_var(init))])))),
  o_let(false, o_var(prod),
    o_app(o_var(short_fold),
      o_fun([o_var(acc), o_var(x)],
        o_tup([
          o_app(o_var(not), o_bop(o_var(x), "=", o_int(0))),
          o_bop(o_var(acc), "*", o_var(x))])),
      o_int(1)),
    o_app(o_var(prod),
      o_list([o_int(3), o_int(4), o_int(5), o_int(0), o_int(6), o_int(7)]))))
).

% Symbol table

% Why prepend? (scope)
sym_add(Table, Sym, Type, [ entry(Sym, Type) | Table ]).

sym_get([ entry(Sym, Type) | _ ], Sym, Type).
sym_get([ _ | Entries ], Sym, Type) :- sym_get(Entries, Sym, Type).

% Constants and literals
% First parameter is symbol table, but we don't need it for these

typeof(_, o_int(_), int).
typeof(_, o_bool(_), bool).

typeof(_, o_tup([]), unit).
typeof(_, o_tup([ E | Es ]), tuple(T, Ts)) :-
    typeof(E, T), typeof(o_tup(Es), Ts).

typeof(_, o_list([]), list(_)).
typeof(_, o_list([ E | Es ]), list(T)) :-
    typeof(E, T), typeof(o_list(Es), list(T)).

typeof(_, o_cons(E1, E2), list(T)) :-
    typeof(E1, T), typeof(E2, list(T)).

% Binary operations

typeof(_, o_bop(E1, Op, E2), int) :-
    (Op = "+"; Op = "*"; Op = "<"), typeof(E1, int), typeof(E2, int).
typeof(_, o_bop(E1, Op, E2), bool) :-
    (Op = "||"; Op = "&&"), typeof(E1, bool), typeof(E2, bool).
typeof(_, o_bop(E1, "=", E2), T) :-
    typeof(E1, T), typeof(E2, T).

% If statements

typeof(_, o_if(Econd, Eif, Eelse), T) :-
    typeof(Econd, bool), typeof(Eif, T), typeof(Eelse, T).

% Function application

typeof(_, o_app(Efun, [ Earg ]), Tret) :-
    typeof(Efun, fun(Targ, Tret)), typeof(Earg, Targ).
typeof(_, o_app(Efun, [ Earg | Eargs ]), Tret) :-
    typeof(_, o_app(o_app(Efun, [ Earg ]), Eargs), Tret).

% Variable names

typeof(SymTable, o_var(Sym), T) :-
    sym_get(SymTable, Sym, T).

% 
