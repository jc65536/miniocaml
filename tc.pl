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

% TODO: write a base case for the type rule for list

typeof(SymTable, list([E | Es]), list(T)) :-
    typeof(SymTable, E, T),
    typeof(SymTable, list(Es), list(T)).

% TODO: write a type rule for cons

% Binary operations

typeof(SymTable, bop(E1, Op, E2), int) :-
    (Op = "+"; Op = "*"; Op = "<"),
    typeof(SymTable, E1, int),
    typeof(SymTable, E2, int).

% TODO: write type rules for ||, &&, and =

% If statements

% TODO

% Function application

% TODO

% Variable names

typeof(SymTable, var(S), T) :-
    st_get(SymTable, S, T).

% Let bindings

% TODO: write type rules for recursive and normal let bindings

% Only variables are allowed as left-hand side of let rec

% Lambdas

% TODO

% Match expressions

% TODO: use the helper predicate type_branches to write a type rule for match
% expressions

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

% TODO
