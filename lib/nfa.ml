type state = int [@@deriving show]

let next_id: state ref = ref 0

type char_class = {
  negated : bool;
  ranges  : (char * char) list;
} [@@deriving show]

type transition =
  | Epsilon
  | Char of char
  | Any
  | Class of char_class
  | Save of int
  [@@deriving show]

let accepts on c =
  match on with
  | Epsilon | Save _ -> false
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
  repr   : string;
  prec   : int;
  start  : state;
  accept : state;
  edges  : edge list;
} [@@deriving show]

let wrap ctx g =
  if g.prec < ctx then "(" ^ g.repr ^ ")" else g.repr

let get_id () =
  let id = !next_id in
  next_id := !next_id + 1;
  id

let atom repr on =
  let src = get_id () in
  let dst = get_id () in
  {
    repr;
    prec = 3;
    start = src;
    accept = dst;
    edges = [ { src; dst; on } ];
  }

let literal c = atom (String.make 1 c) (Char c)

let any () = atom "." Any

let char_class negated ranges =
  let show_range (lo, hi) =
    if lo = hi then String.make 1 lo
    else Printf.sprintf "%c-%c" lo hi
  in
  let repr =
    "[" ^ (if negated then "^" else "")
    ^ String.concat "" (List.map show_range ranges) ^ "]"
  in
  atom repr (Class { negated; ranges })

let group k g =
  let s = get_id () in
  let e = get_id () in
  {
    repr = "(" ^ g.repr ^ ")";
    prec = 3;
    start = s;
    accept = e;
    edges =
      { src = s;        dst = g.start; on = Save (2 * k) } ::
      { src = g.accept; dst = e;       on = Save (2 * k + 1) } ::
      g.edges
  }

let concat g1 g2 =
  {
    repr = wrap 1 g1 ^ wrap 1 g2;
    prec = 1;
    start = g1.start;
    accept = g2.accept;
    edges =
      { src = g1.accept; dst = g2.start; on = Epsilon } ::
      g1.edges @ g2.edges
  }

let concat3 g1 g2 g3 =
  let g1g2 = concat g1 g2 in
  concat g1g2 g3

(* Greedy vs lazy is just the order of the fork edges: drift explores a
   node's edges in order and first-to-a-node wins, so "match more" first
   is greedy, "match less" first is lazy. *)
let star ?(is_lazy = false) g =
  let s = get_id () in
  let e = get_id () in
  let enter = { src = s;        dst = g.start; on = Epsilon } in
  let skip  = { src = s;        dst = e;       on = Epsilon } in
  let loop  = { src = g.accept; dst = g.start; on = Epsilon } in
  let exit  = { src = g.accept; dst = e;       on = Epsilon } in
  {
    repr = wrap 2 g ^ (if is_lazy then "*?" else "*");
    prec = 2;
    start = s;
    accept = e;
    edges =
      (if is_lazy then [ skip; enter; exit; loop ]
       else          [ enter; skip; loop; exit ])
      @ g.edges
  }

let bar g1 g2 =
  let s = get_id () in
  let e = get_id () in
  {
    repr = wrap 0 g1 ^ "|" ^ wrap 0 g2;
    prec = 0;
    start = s;
    accept = e;
    edges =
      { src = s;         dst = g1.start; on = Epsilon } ::
      { src = s;         dst = g2.start; on = Epsilon } ::
      { src = g1.accept; dst = e;        on = Epsilon } ::
      { src = g2.accept; dst = e;        on = Epsilon } ::
      g1.edges @ g2.edges
  }

let plus ?(is_lazy = false) g =
  let s = get_id () in
  let e = get_id () in
  let enter = { src = s;        dst = g.start; on = Epsilon } in
  let loop  = { src = g.accept; dst = g.start; on = Epsilon } in
  let exit  = { src = g.accept; dst = e;       on = Epsilon } in
  {
    repr = wrap 2 g ^ (if is_lazy then "+?" else "+");
    prec = 2;
    start = s;
    accept = e;
    edges =
      enter ::
      (if is_lazy then [ exit; loop ] else [ loop; exit ])
      @ g.edges
  }

let optional ?(is_lazy = false) g =
  let s = get_id () in
  let e = get_id () in
  let try_ = { src = s;        dst = g.start; on = Epsilon } in
  let skip = { src = s;        dst = e;       on = Epsilon } in
  let exit = { src = g.accept; dst = e;       on = Epsilon } in
  {
    repr = wrap 2 g ^ (if is_lazy then "??" else "?");
    prec = 2;
    start = s;
    accept = e;
    edges =
      (if is_lazy then [ skip; try_ ] else [ try_; skip ])
      @ (exit :: g.edges)
  }
