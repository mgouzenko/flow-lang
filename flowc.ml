open Ast;;

type action = Ast | Compile

let _ =
  let action, file =
      if Array.length Sys.argv > 2 then
          (List.assoc Sys.argv.(1) [ ("-a", Ast); ("-c", Compile);],
          open_in Sys.argv.(2))
      else (Compile, open_in Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel file in
  let program = Parser.program Scanner.token lexbuf in
  match action with
  | Ast -> ignore(
      let _ = Printer.print_string_of_program program in
      let graph = "digraph G{" ^ !Printer.dot_graph ^ "}" in
      let outfile = open_out "out.dot" in
      let _ = Printf.fprintf outfile "%s" graph in
      let _ = close_out outfile in
      Sys.command ("dot -Tpng out.dot -o out.png"))
  | Compile ->  Compile.compile program
