parse_tree(
fun(cons(var(x), var(xs)),
  fun(var(y), let(false, any, bop(var(x), "+", var(y)), var(xs))))
).

% Symbol table

% Why prepend? (scope)
st_add(SymTable, Sym, Type, [entry(Sym, Type) | SymTable]).

st_get(SymTable, Sym, Type) :-
    member(entry(Sym, Type), SymTable).

st_eq(SymTable1, SymTable2) :-
    subtract(SymTable1, SymTable2, []).

% Type checking

typeof(E, T) :- typeof([], E, T).

% Constants and literals
% First parameter is symbol table, but we don't need it for these

typeof(_, int(_), int).

typeof(_, bool(_), bool).

typeof(_, tuple([]), unit).

typeof(SymTable, tuple([E | Es]), tuple(T, Ts)) :-
    typeof(SymTable, E, T),
    typeof(SymTable, tuple(Es), Ts).

typeof(_, list([]), list(_)).

typeof(SymTable, list([E | Es]), list(T)) :-
    typeof(SymTable, E, T),
    typeof(SymTable, list(Es), list(T)).

typeof(SymTable, cons(E1, E2), list(T)) :-
    typeof(SymTable, E1, T), typeof(SymTable, E2, list(T)).

% Binary operations

typeof(SymTable, bop(E1, Op, E2), int) :-
    (Op = "+"; Op = "*"; Op = "<"),
    typeof(SymTable, E1, int),
    typeof(SymTable, E2, int).

typeof(SymTable, bop(E1, Op, E2), bool) :-
    (Op = "||"; Op = "&&"),
    typeof(SymTable, E1, bool),
    typeof(SymTable, E2, bool).

typeof(SymTable, bop(E1, "=", E2), bool) :-
    typeof(SymTable, E1, T),
    typeof(SymTable, E2, T).

% If statements

typeof(SymTable, if(Econd, Eif, Eelse), T) :-
    typeof(SymTable, Econd, bool),
    typeof(SymTable, Eif, T),
    typeof(SymTable, Eelse, T).

% Function application

typeof(SymTable, app(Efun, [Earg]), Tret) :-
    typeof(SymTable, Efun, fun(Tparam, Tret)),
    typeof(SymTable, Earg, Tparam).

typeof(SymTable, app(Efun, [Earg | Eargs]), Tret) :-
    typeof(SymTable, app(app(Efun, [Earg]), Eargs), Tret).

% Variable names

typeof(SymTable, var(S), T) :-
    st_get(SymTable, S, T).

% Let bindings

% Only variables are allowed as left-hand side of let rec
typeof(SymTable, let(true, var(S), Ebound, Eout), Tout) :-
    patbind(SymTable, var(S), Trec, NewSymTable),
    typeof(NewSymTable, Ebound, Trec),
    typeof(NewSymTable, Eout, Tout).

typeof(SymTable, let(false, P, Ebound, Eout), Tout) :-
    typeof(SymTable, Ebound, Tbound),
    patbind(SymTable, P, Tbound, NewSymTable),
    typeof(NewSymTable, Eout, Tout).

% Lambdas

typeof(SymTable, fun(P, E), fun(Tparam, Tret)) :-
    patbind(SymTable, P, Tparam, NewSymTable),
    typeof(NewSymTable, E, Tret).

% Match expressions

typeof(SymTable, match(Escrut, Branches), Tret) :-
    typeof(SymTable, Escrut, Tscrut),
    type_branches(SymTable, Branches, Tscrut, Tret).

type_branches(_, [], _, _).

type_branches(SymTable, [branch(P, Eret) | Branches], Tscrut, Tret) :-
    patbind(SymTable, P, Tscrut, NewSymTable),
    typeof(NewSymTable, Eret, Tret),
    type_branches(SymTable, Branches, Tscrut, Tret).

% Patterns

patbind(SymTable, var(Sym), T, NewSymTable) :-
    st_add(SymTable, Sym, T, NewSymTable).

patbind(SymTable, any, _, SymTable).

% Pattern constants

patbind(SymTable, int(_), int, SymTable).

patbind(SymTable, bool(_), bool, SymTable).

patbind(SymTable, tuple([]), unit, SymTable).

patbind(SymTable, tuple([P | Ps]), tuple(T, Ts), NewSymTable) :-
    patbind(SymTable, P, T, SymTable1),
    patbind(SymTable1, tuple(Ps), Ts, NewSymTable).

patbind(SymTable, list([]), list(_), SymTable).

patbind(SymTable, list([P | Ps]), list(T), NewSymTable) :-
    patbind(SymTable, P, T, SymTable1),
    patbind(SymTable1, list(Ps), list(T), NewSymTable).

patbind(SymTable, cons(P1, P2), list(T), NewSymTable) :-
    patbind(SymTable, P1, T, SymTable1),
    patbind(SymTable1, P2, list(T), NewSymTable).

patbind(SymTable, or(P1, P2), T, NewSymTable) :-
    patbind([], P1, T, SymTable1),
    patbind([], P2, T, SymTable2),
    st_eq(SymTable1, SymTable2),
    append(SymTable, SymTable1, NewSymTable).
