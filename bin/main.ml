open Regex_engine

let test pattern inputs =
  let re = Parser.parse pattern in
  Printf.printf "\npattern: %s   (parsed: %s)\n" pattern re.Nfa.repr;
  List.iter
    (fun s -> Printf.printf "  %-10s -> %b\n" (Printf.sprintf "%S" s) (Matcher.matches re s))
    inputs

let () =
  test "(a|b)+(cd)*eq?" [ "ae"; "abcdcde"; "e"; "abcde" ];
  test "a.c"            [ "abc"; "axc"; "ac"; "abbc" ];
  test "[a-z]+"         [ "hello"; "Hello"; ""; "abc123" ];
  test "[0-9]+"         [ "42"; "007"; "1a"; "" ];
  test "[^0-9]+"        [ "abc"; "a1c"; "!!" ];
  test "h[ae]llo"       [ "hello"; "hallo"; "hxllo" ]
