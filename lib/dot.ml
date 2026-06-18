open Nfa

module StateSet = Set.Make (Int)

let escape s =
  String.concat ""
    (List.map
       (fun c ->
         match c with
         | '"' -> "\\\""
         | '\\' -> "\\\\"
         | c -> String.make 1 c)
       (List.init (String.length s) (String.get s)))

let label_of on =
  match on with
  | Epsilon -> "ε"
  | Char c -> String.make 1 c
  | Any -> "."
  | Class { negated; ranges } ->
    let range (lo, hi) =
      if lo = hi then String.make 1 lo else Printf.sprintf "%c-%c" lo hi
    in
    "[" ^ (if negated then "^" else "")
    ^ String.concat "" (List.map range ranges) ^ "]"
  | Save n ->
    if n land 1 = 0 then Printf.sprintf "open %d" (n / 2)
    else Printf.sprintf "close %d" (n / 2)

let all_states g =
  List.fold_left
    (fun acc e -> StateSet.add e.src (StateSet.add e.dst acc))
    (StateSet.add g.start (StateSet.singleton g.accept))
    g.edges

let letters i =
  String.make (i / 26 + 1) (Char.chr (Char.code 'A' + (i mod 26)))

let state_labels g =
  let tbl = Hashtbl.create 16 in
  List.iteri (fun i s -> Hashtbl.add tbl s (letters i)) (StateSet.elements (all_states g));
  tbl

let to_dot g =
  let labels = state_labels g in
  let name s = Hashtbl.find labels s in
  let buf = Buffer.create 256 in
  let line fmt = Printf.ksprintf (fun s -> Buffer.add_string buf (s ^ "\n")) fmt in
  line "digraph nfa {";
  line "  rankdir=LR;";
  line "  label=\"%s\";" (escape g.repr);
  line "  __start [shape=point];";
  StateSet.iter
    (fun s ->
      let shape = if s = g.accept then "doublecircle" else "circle" in
      line "  %s [shape=%s];" (name s) shape)
    (all_states g);
  line "  __start -> %s;" (name g.start);
  List.iter
    (fun e ->
      line "  %s -> %s [label=\"%s\"];" (name e.src) (name e.dst) (escape (label_of e.on)))
    g.edges;
  line "}";
  Buffer.contents buf
