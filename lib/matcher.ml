open Nfa

module StateSet = Set.Make (Int)

type thread = {
  node  : state;
  slots : int option array;
}

let outgoing_edges g s =
  List.filter (fun e -> e.src = s) g.edges

let slot_count g =
  List.fold_left
    (fun acc e -> match e.on with Save n -> max acc (n + 1) | _ -> acc)
    2 g.edges

let drift g pos seeds =
  let rec go visited acc = function
    | [] -> List.rev acc
    | th :: rest ->
      if StateSet.mem th.node visited then
        go visited acc rest
      else begin
        let visited = StateSet.add th.node visited in
        let next =
          List.filter_map
            (fun e ->
              match e.on with
              | Epsilon -> Some { th with node = e.dst }
              | Save i ->
                assert (pos >= 0);
                let slots = Array.copy th.slots in
                slots.(i) <- Some pos;
                Some { node = e.dst; slots }
              | Char _ | Any | Class _ -> None)
            (outgoing_edges g th.node)
        in
        go visited (th :: acc) (next @ rest)
      end
  in
  go StateSet.empty [] seeds

let step g c threads =
  List.concat_map
    (fun th ->
      List.filter_map
        (fun e -> if accepts e.on c then Some { th with node = e.dst } else None)
        (outgoing_edges g th.node))
    threads

let find_first_accepted_thread accept_state threads =
  List.find_map
    (fun th -> if th.node = accept_state then Some th.slots else None)
    threads

(* Run the Pike VM. Returns the highest-priority full match's notebook. *)
let exec g input =
  let len = String.length input in
  let init = { node = g.start; slots = Array.make (slot_count g) None } in

  let rec run pos threads =
    let drifted = drift g pos threads in
    if pos = len then
      find_first_accepted_thread g.accept drifted
    else
      let stepped = step g input.[pos] drifted in
      run (pos + 1) stepped

  in
  run 0 [ init ]

let matches g input = exec g input <> None

let group_substring slots input k =
  match slots.(2 * k), slots.(2 * k + 1) with
  | Some a, Some b ->
    assert (0 <= a && a <= b && b <= String.length input);
    Some (String.sub input a (b - a))
  | None, None -> None
  | _ -> assert false
