parse_tree(
).

% Constants and literals

typeof(N, int) :- number(N).
typeof(true, bool).
typeof(false, bool).

typeof(unit, tuple).
typeof(tuple(_, _), tuple).

typeof([], list).
typeof([ _ | _ ], list).

% Basic operations
