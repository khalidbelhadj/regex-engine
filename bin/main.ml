open Regex_engine

let () =
  print_string (Dot.to_dot (Parser.parse "(ab)+"))
