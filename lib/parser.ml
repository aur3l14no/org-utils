open Angstrom
open Org
open Base

let is_star = Char.equal '*'

let is_newline = Char.equal '\n'

let line =
  take_till is_newline >>= fun text ->
  at_end_of_input >>= function
  | true -> if String.equal text "" then fail "end_of_input" else return text
  | false -> string "\n" >>= fun nl -> return (text ^ nl)

let heading_parser =
  take_while1 is_star >>= fun stars ->
  char ' ' >>= fun _ ->
  line >>= fun text -> return (`Heading { level = String.length stars; text })

let text_parser = line >>= fun text -> return (`Text text)

let parsers = [ heading_parser; text_parser ]

let parse text =
  let org = parse_string ~consume:All (many (choice parsers)) text in
  match org with Ok v -> Some v | Error msg -> failwith msg

let transform_level (lines : line list) =
  let rec match_lines ~lines ~cur_heading_level ~cur_heading_text
      ~cur_content_text ~cur_children : org * line list =
    let finish () =
      let node =
        {
          level = cur_heading_level;
          heading_text = cur_heading_text;
          content_text = List.rev cur_content_text |> String.concat;
        }
      in
      ({ node; children = List.rev cur_children }, lines)
    in
    match lines with
    | [] -> finish ()
    | line :: rest -> (
        match line with
        | `Text text ->
            match_lines ~lines:rest ~cur_heading_level ~cur_heading_text
              ~cur_content_text:(text :: cur_content_text) ~cur_children
        | `Heading { level; text } ->
            if level > cur_heading_level then
              let child, lines =
                match_lines ~lines:rest
                  ~cur_heading_level:(cur_heading_level + 1)
                  ~cur_heading_text:text ~cur_content_text:[] ~cur_children:[]
              in
              match_lines ~lines ~cur_heading_level ~cur_heading_text
                ~cur_content_text ~cur_children:(child :: cur_children)
            else finish ())
  in
  let org, _ =
    match_lines ~lines ~cur_heading_level:0 ~cur_heading_text:""
      ~cur_content_text:[] ~cur_children:[]
  in
  org
