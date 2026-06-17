type state = int [@@deriving show]

let next_id: state ref = ref 0

type char_class = {
  negated : bool;
  ranges  : (char * char) list;   (* a single char 'a' is the range ('a','a') *)
} [@@deriving show]

type transition =
  | Epsilon
  | Char of char
  | Any                 (* '.'  — any single character *)
  | Class of char_class (* '[...]' — membership in a set of ranges *)
  [@@deriving show]

(* Does a non-epsilon transition fire on input character [c]? *)
let accepts (on : transition) (c : char) : bool =
  match on with
  | Epsilon -> false
  | Char c' -> c = c'
  | Any -> true
  | Class { negated; ranges } ->
    let member = List.exists (fun (lo, hi) -> lo <= c && c <= hi) ranges in
    if negated then not member else member

type edge = {
  src : state;
  dst : state;
  on  : transition;
} [@@deriving show]

type t = {
  repr   : string;   (* pretty-printed source, e.g. "(a|b)+" *)
  prec   : int;
  start  : state;
  accept : state;
  edges  : edge list;
} [@@deriving show]

(* Wrap a child's repr in parens only if it binds looser than the
   surrounding context requires. The top-level expression is never a
   child, so it is never wrapped. *)
let wrap (ctx: int) (g: t): string =
  if g.prec < ctx then "(" ^ g.repr ^ ")" else g.repr

let get_id: unit -> state = fun () ->
  let id = !next_id in
  next_id := !next_id + 1;
  id

(* An atom: a fresh src --on--> dst fragment with the given repr. *)
let atom (repr: string) (on: transition): t =
  let src = get_id () in
  let dst = get_id () in
  {
    repr;
    prec = 3;
    start = src;
    accept = dst;
    edges = [ { src; dst; on } ];
  }

let literal (c: char): t = atom (String.make 1 c) (Char c)

let any (): t = atom "." Any

let char_class (negated: bool) (ranges: (char * char) list): t =
  let show_range (lo, hi) =
    if lo = hi then String.make 1 lo
    else Printf.sprintf "%c-%c" lo hi
  in
  let repr =
    "[" ^ (if negated then "^" else "")
    ^ String.concat "" (List.map show_range ranges) ^ "]"
  in
  atom repr (Class { negated; ranges })

let concat (g1: t) (g2: t): t =
  {
    repr = wrap 1 g1 ^ wrap 1 g2;
    prec = 1;
    start = g1.start;
    accept = g2.accept;
    edges =
      { src = g1.accept; dst = g2.start; on = Epsilon } ::
      g1.edges @ g2.edges
  }

let concat3 (g1: t) (g2: t) (g3: t): t =
  let g1g2 = concat g1 g2 in
  concat g1g2 g3

let star (g: t): t =
  let s = get_id () in
  let e = get_id () in
  {
    repr = wrap 2 g ^ "*";
    prec = 2;
    start = s;
    accept = e;
    edges =
      (* We have at least one *)
      { src = s;        dst = g.start; on = Epsilon } ::
      (* Empty *)
      { src = s;        dst = e;       on = Epsilon } ::
      (* Loop back *)
      { src = g.accept; dst = g.start; on = Epsilon } ::
      (* No more to loop *)
      { src = g.accept; dst = e;       on = Epsilon } ::
      g.edges
  }

let bar (g1: t) (g2: t): t =
  let s = get_id () in
  let e = get_id () in
  {
    repr = wrap 0 g1 ^ "|" ^ wrap 0 g2;
    prec = 0;
    start = s;
    accept = e;
    edges =
      (* Matching left graph *)
      { src = s;         dst = g1.start; on = Epsilon } ::
      (* Matching right graph *)
      { src = s;         dst = g2.start; on = Epsilon } ::
      { src = g1.accept; dst = e;        on = Epsilon } ::
      { src = g2.accept; dst = e;        on = Epsilon } ::
      g1.edges @ g2.edges
  }

let plus (g: t): t =
  let s = get_id () in
  let e = get_id () in
  {
    repr = wrap 2 g ^ "+";
    prec = 2;
    start = s;
    accept = e;
    edges =
      (* We have at least one *)
      { src = s;        dst = g.start; on = Epsilon } ::
      (* Loop back *)
      { src = g.accept; dst = g.start; on = Epsilon } ::
      (* No more to loop *)
      { src = g.accept; dst = e;       on = Epsilon } ::
      g.edges
  }

let optional (g: t): t =
  let s = get_id () in
  let e = get_id () in
  {
    repr = wrap 2 g ^ "?";
    prec = 2;
    start = s;
    accept = e;
    edges =
      (* It exists *)
      { src = s;        dst = g.start; on = Epsilon } ::
      (* It doesn't exist *)
      { src = s;        dst = e;       on = Epsilon } ::
      (* Wire up the end states *)
      { src = g.accept; dst = e;       on = Epsilon } ::
      g.edges
  }
