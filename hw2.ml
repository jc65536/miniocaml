open List
open Printf

type nonterm = Expr | ExprTupleRep | ExprListRep | ArgRep | RecOpt | InfixOp |
Constant | PatternMatching | BranchRep | Pattern | PatternTupleRep |
PatternListRep | LetBinding | ParamRep

let nonterm_str = function
    | Expr -> "expr"
    | ExprTupleRep -> "expr_tuple_rep"
    | ExprListRep -> "expr_list_rep"
    | ArgRep -> "arg_rep"
    | RecOpt -> "rec_opt"
    | InfixOp -> "infix_op"
    | Constant -> "constant"
    | PatternMatching -> "pattern_matching"
    | BranchRep -> "branch_rep"
    | Pattern -> "pattern"
    | PatternTupleRep -> "pattern_tuple_rep"
    | PatternListRep -> "pattern_list_rep"
    | LetBinding -> "let_binding"
    | ParamRep -> "param_rep"

type symbol = N of nonterm | S of string | F of (string -> bool)

type parse_tree =
    | Node of nonterm * (parse_tree list)
    | Leaf of string

let integer_literal s = String.length s > 0 && String.for_all (String.contains "0123456789") s
let value_name s = String.length s > 0 && String.for_all (String.contains "abcdefghijklmnopqrstuvwxyz_") s

let gram: nonterm * (nonterm -> symbol list list) = Expr, function
    | Expr -> [
        [N Constant];
        [S "("; N Expr; S ")"];
        [S "["; N Expr; N ExprListRep; S "]"];
        [S "if"; N Expr; S "then"; N Expr; S "else"; N Expr];
        [S "match"; N Expr; S "with"; N PatternMatching];
        [S "fun"; N Pattern; S "->"; N Expr];
        [S "let"; N RecOpt; N LetBinding; S "in"; N Expr];
        [N Expr; N ExprTupleRep];
        [N Expr; S "::"; N Expr];
        [N Expr; N ArgRep];
        [N Expr; N InfixOp; N Expr];
        ]
    | ExprTupleRep -> [[S ","; N Expr];
                       [S ","; N Expr; N ExprTupleRep]]
    | ExprListRep -> [[]; [S ";"; N Expr; N ExprListRep]]
    | ArgRep -> [[N Expr]; [N Expr; N ArgRep]]
    | RecOpt -> [[]; [S "rec"]]
    | InfixOp -> [[S "+"]; [S "*"]; [S "="]; [S "<"]; [S "||"]; [S "&&"]]
    | Constant -> [[F integer_literal]; [S "false"];
                   [S "true"]; [S "()"]; [S "[]"]]
    | PatternMatching -> [[N Pattern; S "->"; N Expr; N BranchRep]]
    | BranchRep -> [[]; [S "|"; N Pattern; S "->"; N Expr; N BranchRep]]
    | Pattern -> [
        [F value_name];
        [S "_"];
        [N Constant];
        [S "("; N Pattern; S ")"];
        [S "["; N Pattern; N PatternListRep; S "]"];
        [N Pattern; N PatternTupleRep];
        [N Pattern; S "::"; N Pattern]
        ]
    | PatternTupleRep -> [[S ","; N Pattern]; [S ","; N Pattern; N PatternTupleRep]]
    | PatternListRep -> [[]; [S ";"; N Pattern; N PatternListRep]]
    | LetBinding -> [
        [F value_name; N ParamRep; S "="; N Expr];
        [N Pattern; S "="; N Expr];
        ]
    | ParamRep -> [[]; [N Pattern; N ParamRep]]

let make_parser gram frag = let start, prodfun = gram in
    let rec match_sym accept sym frag = match sym, frag with
        (* sym is nonterminal => tries to match sym to a prefix of frag by
           trying all rules for sym in order *)
        | N nt, _ -> printf "Nonterm: %s\n" (nonterm_str nt); (match match_rules accept (prodfun nt) frag with
            (* stack contains at least two lists: the children list of sym and
               sibling list of sym *)
            | Some (chil::sib::rest) -> printf "ok\n"; Some ((Node (nt, chil)::sib)::rest)
            (* stack contains one element: the children list of sym *)
            | Some [children] -> printf "ok\n"; Some [[Node (nt, children)]]
            | _ -> printf "%s backtrack\n" (nonterm_str nt); None)

        (* sym is terminal and frag has tokens => sym must equal first token *)
        | S t, fh::ft when t = fh -> printf "Term: %s %s\n" t fh; (match accept ft with
            (* prepend Leaf to sibling list *)
            | Some (sib::rest) -> printf "ok\n"; Some ((Leaf t::sib)::rest)
            | _ -> printf "%s %s backtrack\n" t fh; None)
        
        | F p, fh::ft when p fh -> printf "Term: regex %s\n" fh; (match accept ft with
            | Some (sib::rest) -> printf "ok\n"; Some ((Leaf fh::sib)::rest)
            | _ -> printf "regex %s backtrack\n" fh; None)

        (* sym is terminal and first token in frag doesn't match or frag is
           empty => reject *)
        | _ -> None

    and match_rule accept rule frag = match rule with
        (* rule has symbols => match the first symbol if possible, and accept
           only if the rest of the rule can match the suffix *)
        | rh::rt -> match_sym (match_rule accept rt) rh frag

        (* rule has no symbols => matches anything *)
        | _ -> (match accept frag with
                (* push new list onto stack to hold parse trees of each symbol
                   in rule - this part is confusing because the current rule is
                   empty, but the list is actually prepended to on the way back
                   up the call stack *)
                | Some stack -> Some ([]::stack)
                | _ -> None)

    and match_rules accept rules frag =
        find_map (fun rule -> match_rule accept rule frag) rules

    in match match_sym (function [] -> Some [] | _ -> None) (N start) frag with
    | Some [[tree]] -> Some tree
    | _ -> None

let tokens = [
    "let"; "rec"; "short_fold"; "f"; "init"; "list"; "="; "match"; "list";
    "with"; "|"; "x"; "::"; "xs"; "->"; "let"; "("; "cont"; ","; "acc"; ")";
    "="; "f"; "init"; "x"; "in"; "if"; "cont"; "then"; "short_fold"; "f"; "acc";
    "xs"; "else"; "acc"; "|"; "_"; "->"; "init"; "in"; "let"; "prod"; "=";
    "short_fold"; "("; "fun"; "acc"; "x"; "->"; "("; "not"; "("; "x"; "="; "0";
    ")"; ","; "acc"; "*"; "x"; ")"; ")"; "1"; "in"; "prod"; "["; "3"; ";"; "4";
    ";"; "5"; ";"; "0"; ";"; "6"; ";"; "7"; "]"
]

let rec print_tree = function
    | Node (nt, trees) -> print_string (nonterm_str nt);
        print_string "(";
        iter (fun t -> print_tree t; print_string ",") trees;
        print_string ")"
    | Leaf (str) -> printf "\"%s\"" str
