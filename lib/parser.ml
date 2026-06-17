(* Recursive descent parser: turns a regex string into an NFA graph.

   Grammar (lowest precedence first):
     expr   := term ('|' term)*
     term   := factor+
     factor := atom ('*' | '+' | '?')*
     atom   := CHAR | '.' | class | '(' expr ')'
     class  := '[' '^'? item+ ']'
     item   := CHAR '-' CHAR | CHAR
*)

type cursor = {
  rest  : char list;
  group : int;
}

let rec parse_expr (c : cursor) : Nfa.t * cursor =
  let (left, c) = parse_term c in
  let rec loop acc c =
    match c.rest with
    | '|' :: rest ->
      let (right, c) = parse_term { c with rest } in
      loop (Nfa.bar acc right) c
    | _ -> (acc, c)
  in
  loop left c

and parse_term (c : cursor) : Nfa.t * cursor =
  let (first, c) = parse_factor c in
  let rec loop acc c =
    match c.rest with
    | [] | '|' :: _ | ')' :: _ -> (acc, c)
    | _ ->
      let (g, c) = parse_factor c in
      loop (Nfa.concat acc g) c
  in
  loop first c

and parse_factor (c : cursor) : Nfa.t * cursor =
  let (a, c) = parse_atom c in
  let rec apply g = function
    | '*' :: '?' :: rest -> apply (Nfa.star ~is_lazy:true g) rest
    | '*' :: rest        -> apply (Nfa.star g) rest
    | '+' :: '?' :: rest -> apply (Nfa.plus ~is_lazy:true g) rest
    | '+' :: rest        -> apply (Nfa.plus g) rest
    | '?' :: '?' :: rest -> apply (Nfa.optional ~is_lazy:true g) rest
    | '?' :: rest        -> apply (Nfa.optional g) rest
    | rest               -> (g, rest)
  in
  let (g, rest) = apply a c.rest in
  (g, { c with rest })

and parse_atom (c : cursor) : Nfa.t * cursor =
  match c.rest with
  | '(' :: rest ->
    let k = c.group in
    let (g, c) = parse_expr { rest; group = c.group + 1 } in
    (match c.rest with
     | ')' :: rest -> (Nfa.group k g, { c with rest })
     | _ -> failwith "parse_atom: expected ')'")
  | '.' :: rest -> (Nfa.any (), { c with rest })
  | '[' :: rest -> parse_class { c with rest }
  | ch :: rest when not (List.mem ch [ '|'; '*'; '+'; '?'; '('; ')'; '.'; '['; ']' ]) ->
    (Nfa.literal ch, { c with rest })
  | _ -> failwith "parse_atom: expected a character, '.', '[' or '('"

and parse_class (c : cursor) : Nfa.t * cursor =
  let (negated, input) =
    match c.rest with
    | '^' :: rest -> (true, rest)
    | rest -> (false, rest)
  in
  let rec items acc = function
    | ']' :: rest -> (List.rev acc, rest)
    | lo :: '-' :: hi :: rest when hi <> ']' -> items ((lo, hi) :: acc) rest
    | ch :: rest -> items ((ch, ch) :: acc) rest
    | [] -> failwith "parse_class: unterminated '['"
  in
  let (ranges, rest) = items [] input in
  (Nfa.char_class negated ranges, { c with rest })

let parse (input : string) : Nfa.t =
  let chars = List.init (String.length input) (String.get input) in
  (* user groups start at 1; group 0 wraps the whole match *)
  let (g, c) = parse_expr { rest = chars; group = 1 } in
  match c.rest with
  | [] -> Nfa.group 0 g
  | _ -> failwith "parse: unexpected trailing input"
