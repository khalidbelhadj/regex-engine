open Regex_engine

(* --- tiny test harness: collect all failures, exit non-zero if any --- *)

let checks = ref 0
let failures = ref 0

let green s = "\027[32m" ^ s ^ "\027[0m"
let red s   = "\027[31m" ^ s ^ "\027[0m"

let check name cond =
  incr checks;
  if cond then
    Printf.printf "  %s  %s\n" (green "PASS") name
  else begin
    incr failures;
    Printf.printf "  %s  %s\n" (red "FAIL") name
  end

(* --- helpers over the engine --- *)

let matches pat s = Matcher.matches (Parser.parse pat) s

(* Parse [pat], match [s], and return the captured groups as a list of
   string options (group 0 first). None if no match. *)
let groups pat s =
  match Matcher.exec (Parser.parse pat) s with
  | None -> None
  | Some slots ->
    let n = Array.length slots / 2 in
    Some (List.init n (fun k -> Matcher.group_substring slots s k))

(* --- boolean matching --- *)

let () =
  check "literal match"         (matches "a" "a");
  check "literal mismatch"      (not (matches "a" "b"));
  check "concat"                (matches "abc" "abc");
  check "concat anchored end"   (not (matches "ab" "abc"));
  check "concat anchored start" (not (matches "bc" "abc"));

  check "alt left"              (matches "a|b" "a");
  check "alt right"             (matches "a|b" "b");
  check "alt none"              (not (matches "a|b" "c"));

  check "star zero"             (matches "a*" "");
  check "star many"             (matches "a*" "aaaa");
  check "plus zero"             (not (matches "a+" ""));
  check "plus one"              (matches "a+" "a");
  check "opt absent"            (matches "ab?c" "ac");
  check "opt present"           (matches "ab?c" "abc");
  check "opt at most one"       (not (matches "ab?c" "abbc"))

let () =
  check "dot one"               (matches "a.c" "axc");
  check "dot needs a char"      (not (matches "a.c" "ac"));
  check "class range"           (matches "[a-z]+" "hello");
  check "class range neg case"  (not (matches "[a-z]+" "Hello"));
  check "class set"             (matches "h[ae]llo" "hallo");
  check "class set miss"        (not (matches "h[ae]llo" "hxllo"));
  check "class negated"         (matches "[^0-9]+" "abc");
  check "class negated miss"    (not (matches "[^0-9]+" "a1c"));

  check "nested groups star"    (matches "(a(b|c)*d)+" "abcdacd");
  check "complex"               (matches "(a|b)+(cd)*e" "abcdcde")

(* --- capture groups --- *)

let () =
  check "captures digits"
    (groups "([0-9]+)-([0-9]+)" "12-34"
     = Some [ Some "12-34"; Some "12"; Some "34" ]);

  check "no match -> None"
    (groups "([0-9]+)-([0-9]+)" "12-" = None);

  check "alternation priority"
    (groups "(a|ab)(c|bc)" "abc"
     = Some [ Some "abc"; Some "a"; Some "bc" ]);

  check "dead thread loses"
    (groups "(ab|a)b" "ab" = Some [ Some "ab"; Some "a" ]);

  check "optional group set"
    (groups "(a)?b" "ab" = Some [ Some "ab"; Some "a" ]);

  check "optional group unset"
    (groups "(a)?b" "b" = Some [ Some "b"; None ]);

  check "repeated group keeps last"
    (groups "(a(b)c)+" "abcabc"
     = Some [ Some "abcabc"; Some "abc"; Some "b" ]);

  check "whole match is group 0"
    (groups "a.c" "axc" = Some [ Some "axc" ]);

  (* engine is full-match: substring-only matches are rejected *)
  check "no substring: literal" (not (matches "b" "abc"));
  check "no substring: prefix"  (not (matches "ab" "abcdef"));
  check "no substring: suffix"  (not (matches "ef" "abcdef"))

(* --- greedy vs lazy (observable via how two groups split the input) --- *)

let () =
  check "greedy: first group grabs all"
    (groups "(a*)(a*)" "aaa" = Some [ Some "aaa"; Some "aaa"; Some "" ]);

  check "lazy: first group grabs none"
    (groups "(a*?)(a*)" "aaa" = Some [ Some "aaa"; Some ""; Some "aaa" ]);

  check "lazy plus still matches (full-match)" (matches "a+?" "aaa");
  check "lazy plus needs one"                  (not (matches "a+?" ""))

(* --- summary --- *)

let () =
  let summary = Printf.sprintf "%d checks, %d failure(s)" !checks !failures in
  Printf.printf "\n%s\n" (if !failures = 0 then green summary else red summary);
  if !failures > 0 then exit 1
