open Mylib.Org
open Base
open Stdio

let filename = "examples/20201109"

(* let filenames = [ "examples/20201109"; "examples/20201110" ] *)

let filenames =
  [
    "/Users/y/Dropbox/Org/journal/20201109";
    "/Users/y/Dropbox/Org/journal/20201110";
    "/Users/y/Dropbox/Org/journal/20201111";
    "/Users/y/Dropbox/Org/journal/20201112";
    "/Users/y/Dropbox/Org/journal/20201116";
    "/Users/y/Dropbox/Org/journal/20201120";
    "/Users/y/Dropbox/Org/journal/20201126";
    "/Users/y/Dropbox/Org/journal/20201202";
  ]

let transform = Mylib.Parser.transform_level

let convert_org_journal_daily_to_classical_monthly (org : org) : org =
  match org with
  | {
   node = { level = 0; _ };
   children = [ { node = { heading_text = date_string; _ }; _ } ];
  } ->
      Core.Date.(
        let date_string =
          date_string |> String.rstrip |> String.split ~on:' ' |> List.rev
          |> List.hd_exn
        in
        let date = of_string date_string in
        let daily_org = shift_org_node (List.hd_exn org.children) ~l:2 in
        let new_daily_org =
          {
            node =
              {
                level = 3;
                heading_text = format date "%Y-%m-%d %A\n";
                content_text = "\n";
              };
            children = daily_org.children;
          }
        in
        nest_mtree
          (List.mapi [ "%Y"; "%Y-%m %B" ] ~f:(fun i s ->
               {
                 level = i + 1;
                 heading_text = format date s ^ "\n";
                 content_text = "\n";
               }))
          new_daily_org)
  | _ -> failwith "Not an org-journal-daily file!"

let process filename =
  let content = Mylib.Helper.read_file filename ^ "\n" in
  let parsed_org = Mylib.Parser.parse content in
  match parsed_org with
  | None -> None
  | Some v ->
      Some (transform v |> convert_org_journal_daily_to_classical_monthly)

(* let () =
   let org = parse filename in
   match org with None -> () | Some v -> print_string (Mylib.Exporter.export v) *)

let () =
  List.filter_map ~f:process filenames
  |> merge_mtrees |> List.hd_exn |> Mylib.Exporter.export |> print_string

(* let () =  *)

(* let () =
   let org = *)
