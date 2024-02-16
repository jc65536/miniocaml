parse_tree(
o_let(false, o_or(o_list([o_var(a), o_int(4)]), o_cons(o_var(a), o_list([]))),
  o_list([o_int(2), o_int(4)]), o_var(a))
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

typeof(_, o_int(_), int).

typeof(_, o_bool(_), bool).

typeof(_, o_tup([]), unit).

typeof(SymTable, o_tup([E | Es]), tuple(T, Ts)) :-
    typeof(SymTable, E, T),
    typeof(SymTable, o_tup(Es), Ts).

typeof(_, o_list([]), list(_)).

typeof(SymTable, o_list([E | Es]), list(T)) :-
    typeof(SymTable, E, T),
    typeof(SymTable, o_list(Es), list(T)).

typeof(SymTable, o_cons(E1, E2), list(T)) :-
    typeof(SymTable, E1, T), typeof(SymTable, E2, list(T)).

% Binary operations

typeof(SymTable, o_bop(E1, Op, E2), int) :-
    (Op = "+"; Op = "*"; Op = "<"),
    typeof(SymTable, E1, int),
    typeof(SymTable, E2, int).

typeof(SymTable, o_bop(E1, Op, E2), bool) :-
    (Op = "||"; Op = "&&"),
    typeof(SymTable, E1, bool),
    typeof(SymTable, E2, bool).

typeof(SymTable, o_bop(E1, "=", E2), T) :-
    typeof(SymTable, E1, T),
    typeof(SymTable, E2, T).

% If statements

typeof(SymTable, o_if(Econd, Eif, Eelse), T) :-
    typeof(SymTable, Econd, bool),
    typeof(SymTable, Eif, T),
    typeof(SymTable, Eelse, T).

% Function application

typeof(SymTable, o_app(Efun, [Earg]), Tret) :-
    typeof(SymTable, Efun, fun(Tparam, Tret)),
    typeof(SymTable, Earg, Tparam).

typeof(SymTable, o_app(Efun, [Earg | Eargs]), Tret) :-
    typeof(SymTable, o_app(o_app(Efun, [Earg]), Eargs), Tret).

% Variable names

typeof(SymTable, o_var(S), T) :-
    st_get(SymTable, S, T).

% Let bindings

% Only variables are allowed as left-hand side of let rec
typeof(SymTable, o_let(true, o_var(S), Ebound, Eout), Tout) :-
    patbind(SymTable, o_var(S), Trec, NewSymTable),
    typeof(NewSymTable, Ebound, Trec),
    typeof(NewSymTable, Eout, Tout).

typeof(SymTable, o_let(false, P, Ebound, Eout), Tout) :-
    typeof(SymTable, Ebound, Tbound),
    patbind(SymTable, P, Tbound, NewSymTable),
    typeof(NewSymTable, Eout, Tout).

% Lambdas

typeof(SymTable, o_fun(P, E), fun(Tparam, Tret)) :-
    patbind(SymTable, P, Tparam, NewSymTable),
    typeof(NewSymTable, E, Tret).

% Match expressions

typeof(SymTable, o_match(Escrut, Branches), Tret) :-
    typeof(SymTable, Escrut, Tscrut),
    type_branches(SymTable, Branches, Tscrut, Tret).

type_branches(_, [], _, _).

type_branches(SymTable, [o_branch(P, Eret) | Branches], Tscrut, Tret) :-
    patbind(SymTable, P, Tscrut, NewSymTable),
    typeof(NewSymTable, Eret, Tret),
    type_branches(SymTable, Branches, Tscrut, Tret).

% Patterns

patbind(SymTable, o_var(Sym), T, NewSymTable) :-
    st_add(SymTable, Sym, T, NewSymTable).

patbind(SymTable, o_wild, _, SymTable).

% Pattern constants

patbind(SymTable, o_int(_), int, SymTable).

patbind(SymTable, o_bool(_), bool, SymTable).

patbind(SymTable, o_tup([]), unit, SymTable).

patbind(SymTable, o_tup([P | Ps]), tuple(T, Ts), NewSymTable) :-
    patbind(SymTable, P, T, SymTable1),
    patbind(SymTable1, o_tup(Ps), Ts, NewSymTable).

patbind(SymTable, o_list([]), list(_), SymTable).

patbind(SymTable, o_list([P | Ps]), list(T), NewSymTable) :-
    patbind(SymTable, P, T, SymTable1),
    patbind(SymTable1, o_list(Ps), list(T), NewSymTable).

patbind(SymTable, o_cons(P1, P2), list(T), NewSymTable) :-
    patbind(SymTable, P1, T, SymTable1),
    patbind(SymTable1, P2, list(T), NewSymTable).

patbind(SymTable, o_or(P1, P2), T, NewSymTable) :-
    patbind([], P1, T, SymTable1),
    patbind([], P2, T, SymTable2),
    st_eq(SymTable1, SymTable2),
    append(SymTable, SymTable1, NewSymTable).
