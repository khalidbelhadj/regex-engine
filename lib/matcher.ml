open Nfa

module StateSet = Set.Make (Int)

let closure (g: t) (states: StateSet.t): StateSet.t =
  let rec loop seen = function
    | [] -> seen
    | n :: rest ->
      let next =
        List.filter_map
          (fun e ->
            if e.src = n && e.on = Epsilon && not (StateSet.mem e.dst seen)
            then Some e.dst
            else None)
          g.edges
      in
      let seen = List.fold_left (fun acc m -> StateSet.add m acc) seen next in
      loop seen (next @ rest)
  in
  loop states (StateSet.elements states)

let step (g: t) (states: StateSet.t) (c: char): StateSet.t =
  let next =
    List.filter_map
      (fun e ->
        if StateSet.mem e.src states && accepts e.on c
        then Some e.dst
        else None)
      g.edges
  in
  closure g (StateSet.of_list next)

let matches (g: t) (input: string): bool =
  let start = closure g (StateSet.singleton g.start) in
  let final =
    String.fold_left (fun states c -> step g states c) start input
  in
  StateSet.mem g.accept final
