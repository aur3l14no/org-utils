open Org
open Base

let example_org : org =
  {
    node =
      { level = 1; heading_text = "First heading"; content_text = "some text" };
    children =
      [
        {
          node =
            {
              level = 3;
              heading_text = "Another heading";
              content_text = "some text again";
            };
          children = [];
        };
      ];
  }

let rec export (org : org) =
  match org with
  | { node; children } ->
      let heading_exp =
        if node.level > 0 then
          String.make node.level '*' ^ " " ^ node.heading_text
        else ""
      in
      let content_exp = node.content_text in
      let children_exp = String.concat (List.map ~f:export children) in
      String.concat [ heading_exp; content_exp; children_exp ]
