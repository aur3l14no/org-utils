open Base

type org_node = { level : int; heading_text : string; content_text : string }

let poly_eq x y = Stdlib.compare x y = 0

type 'a mtree = { node : 'a; children : 'a mtree list }

type org = org_node mtree

let rec shift_org_node org ~l =
  match org with
  | { node = { level; heading_text; content_text }; children } ->
      {
        node = { level = level + l; heading_text; content_text };
        children = List.map ~f:(shift_org_node ~l) children;
      }

let nest_mtree (nodes : 'a list) (mtree : 'a mtree) =
  let rec f nodes mtree =
    match nodes with
    | [] -> mtree
    | hd :: tl -> f tl { node = hd; children = [ mtree ] }
  in
  f (List.rev nodes) mtree

let rec merge_mtrees (mtrees : 'a mtree list) : 'a mtree list =
  (*
     1. partition mtrees based on their nodes (-> mtree_alist)
     2. merge within partitions
   *)
  let mtree_alist : (org_node * org list) list =
    List.fold mtrees
      ~f:(fun acc x ->
        let existing = List.Assoc.find acc ~equal:poly_eq x.node in
        match existing with
        | Some v -> List.Assoc.add acc x.node (v @ x.children) ~equal:poly_eq
        | None -> (x.node, x.children) :: acc)
      ~init:[]
  in
  mtree_alist |> List.rev
  |> List.map ~f:(fun x -> { node = fst x; children = merge_mtrees (snd x) })

type heading = { level : int; text : string }

type line = [ `Text of string | `Heading of heading ]

(* type paragraph = [ `Text | `Heading ] *)
