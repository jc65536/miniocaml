open List

type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
    | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
    | Leaf of 'terminal

let convert_grammar (start, rules) = start, fun nt ->
    let check_nt (l, r) = if l = nt then Some r else None
    in filter_map check_nt rules

let parse_tree_leaves tree =
    let rec prepend_leaves acc = function
        | Node (_, children) -> fold_left prepend_leaves acc children
        | Leaf t -> t::acc
    in rev (prepend_leaves [] tree)

let make_matcher gram accept frag = let start, prodfun = gram in
    let rec match_sym accept sym frag = match sym, frag with
        (* sym is nonterminal => tries to match sym to a prefix of frag by
           trying all rules for sym in order *)
        | N nt, _ -> match_rules accept (prodfun nt) frag

        (* sym is terminal and frag has tokens => sym must equal first token *)
        | T t, fh::ft when t = fh -> accept ft

        (* sym is terminal and first token in frag doesn't match or frag is
           empty => reject *)
        | _ -> None

    and match_rule accept rule frag = match rule with
        (* rule has symbols => match the first symbol if possible, and accept
           only if the rest of the rule can match the suffix *)
        | rh::rt -> match_sym (match_rule accept rt) rh frag

        (* rule has no symbols => matches anything *)
        | _ -> accept frag

    and match_rules accept rules frag =
        find_map (fun rule -> match_rule accept rule frag) rules

    in match_sym accept (N start) frag

let make_parser gram frag = let start, prodfun = gram in
    (* Acceptor returns an optional stack of parse tree lists. The top parse
       tree list represents a children list of the current nonterminal, in
       other words the parse trees of each symbol in the current rule for the
       nonterminal. A new list is pushed onto the stack when we hit the end of
       a rule. As each recursive call returns, it prepends elements to the top
       list. Once the top list has been completed, a new node is created with
       the top list as its children list, and the new node is prepended onto
       the second list in the stack (sibling list).

       Note that because the stack is only created/manipulated on the way back
       up the call stack, this means no stack exists until we've already found
       a path that matches the entire frag. For any path that doesn't work, the
       ultimate acceptor returns None, so we backtrack and move on without
       creating the stack. *)

    let rec match_sym accept sym frag = match sym, frag with
        (* sym is nonterminal => tries to match sym to a prefix of frag by
           trying all rules for sym in order *)
        | N nt, _ -> (match match_rules accept (prodfun nt) frag with
            (* stack contains at least two lists: the children list of sym and
               sibling list of sym *)
            | Some (chil::sib::rest) -> Some ((Node (nt, chil)::sib)::rest)
            (* stack contains one element: the children list of sym *)
            | Some [children] -> Some [[Node (nt, children)]]
            | _ -> None)

        (* sym is terminal and frag has tokens => sym must equal first token *)
        | T t, fh::ft when t = fh -> (match accept ft with
            (* prepend Leaf to sibling list *)
            | Some (sib::rest) -> Some ((Leaf t::sib)::rest)
            | _ -> None)

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
