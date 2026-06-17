(* Recursive descent parser: turns a regex string into an NFA graph.

   Grammar (lowest precedence first):

     expr   := term ('|' term)*          -> Nfa.bar
     term   := factor+                   -> Nfa.concat
     factor := atom ('*' | '+' | '?')*   -> Nfa.star / plus / optional
     atom   := CHAR | '(' expr ')'       -> Nfa.literal / grouping

   Functional style, no mutation: each rule has type

     char list -> Nfa.t * char list

   i.e. it takes the remaining input and returns (parsed NFA, leftover input).
   The "leftover" is threaded into the next step instead of a mutable cursor. *)

let rec parse_expr (input : char list) : Nfa.t * char list =
  (* term ('|' term)* — fold alternatives together with Nfa.bar *)
  let (left, rest) = parse_term input in
  let rec loop acc = function
    | '|' :: rest ->
      let (right, rest) = parse_term rest in
      loop (Nfa.bar acc right) rest
    | rest -> (acc, rest)
  in
  loop left rest

and parse_term (input : char list) : Nfa.t * char list =
  (* factor+ — concatenate factors until something that can't start an atom *)
  let (first, rest) = parse_factor input in
  let rec loop acc = function
    | [] | '|' :: _ | ')' :: _ as rest -> (acc, rest)
    | rest ->
      let (g, rest) = parse_factor rest in
      loop (Nfa.concat acc g) rest
  in
  loop first rest

and parse_factor (input : char list) : Nfa.t * char list =
  (* Parse an atom, then apply any trailing postfix operators. *)
  let (atom, rest) = parse_atom input in
  let rec apply g = function
    | '*' :: rest -> apply (Nfa.star g) rest
    | '+' :: rest -> apply (Nfa.plus g) rest
    | '?' :: rest -> apply (Nfa.optional g) rest
    | rest        -> (g, rest)
  in
  apply atom rest

and parse_atom (input : char list) : Nfa.t * char list =
  match input with
  | '(' :: rest ->
    let (g, rest) = parse_expr rest in
    (match rest with
     | ')' :: rest -> (g, rest)
     | _ -> failwith "parse_atom: expected ')'")
  | '.' :: rest -> (Nfa.any (), rest)
  | '[' :: rest -> parse_class rest
  | c :: rest when not (List.mem c [ '|'; '*'; '+'; '?'; '('; ')'; '.'; '['; ']' ]) ->
    (Nfa.literal c, rest)
  | _ -> failwith "parse_atom: expected a character, '.', '[' or '('"

(* Called after consuming '['. Parses an optional '^' then class items
   (single chars or 'a-z' ranges) up to and including the closing ']'. *)
and parse_class (input : char list) : Nfa.t * char list =
  let (negated, input) =
    match input with
    | '^' :: rest -> (true, rest)
    | _ -> (false, input)
  in
  let rec items acc = function
    | ']' :: rest -> (List.rev acc, rest)
    | lo :: '-' :: hi :: rest when hi <> ']' -> items ((lo, hi) :: acc) rest
    | c :: rest -> items ((c, c) :: acc) rest
    | [] -> failwith "parse_class: unterminated '['"
  in
  let (ranges, rest) = items [] input in
  (Nfa.char_class negated ranges, rest)

let parse (input : string) : Nfa.t =
  let chars = List.init (String.length input) (String.get input) in
  let (g, rest) = parse_expr chars in
  match rest with
  | [] -> g
  | _ -> failwith "parse: unexpected trailing input"
