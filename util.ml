let ( << ) f g x = f (g x)

type frag = frag Xmlm.frag

module ConStr = struct

  type t =
    | Pair of t * t
    | Cell of string
    | Nil

  let cons a b =
    Pair (a, b)

  let concat sep xs =
    let cons' = (fun a x -> cons (Cell sep) (cons x a)) in
    match List.fold_left cons' Nil (List.rev xs) with
    | Pair (a, b) -> b
    | Cell _ -> failwith "ConStr.concat: Invalid state."
    | Nil -> Nil

  let of_string s =
    Cell s

  let to_string x =
    let b = Buffer.create 16 in
    let rec encode x =
      match x with
      | Pair(a, b) ->
        encode a ;
        encode b
      | Cell s ->
        Buffer.add_string b s
      | Nil -> ()
    in
    let () = 
      encode x
    in Buffer.contents b

end

(* TODO: Compare performance with plain string implementation. *)

let string_of_xmlm (x : frag) =
  let rec string_of_xmlm' x =
    let lt = ConStr.of_string "<" in
    let gt = ConStr.of_string ">" in
    let lts = ConStr.of_string "</" in
    let sgt = ConStr.of_string "/>" in
    let sp = ConStr.of_string " " in
    let string_of_name ns tag =
      match ns, tag with
      | _, "" -> invalid_arg "Names cannot have empty local elements."
      | "", _ -> tag
      | _ -> Printf.sprintf "%s:%s" ns tag
    in
    let rec string_of_attr  ((ns, tag), value) =
      if value = "" then
        Printf.sprintf "%s" (string_of_name ns tag)
      else
        Printf.sprintf "%s=\"%s\""  (string_of_name ns tag) value
    in
    match x with
    | `Data s -> ConStr.of_string s
    | `El  (((ns, tag), attrs), children) ->
      let attrs' = List.map (ConStr.of_string << string_of_attr) attrs |> ConStr.concat " " in
      let tag' = string_of_name ns tag |> ConStr.of_string in
      let children' = List.map string_of_xmlm' children |> ConStr.concat "" in
      match attrs, children with
      | [] , [] -> ConStr.concat "" [lt; tag'; sgt] (* <tag /> *)
      | _, [] -> ConStr.concat "" [lt; tag'; sp; attrs'; sp; sgt] (* <tag attrs /> *)
      | [] , _ -> ConStr.concat "" [lt; tag'; gt; children'; lts; tag'; gt] (* <tag>children</tag> *)
      | _, _ -> ConStr.concat "" [lt; tag'; sp; attrs'; gt; children'; lts; tag'; gt] (* <tag attrs />children</tag> *)
  in string_of_xmlm' x |> ConStr.to_string

let html_of_sxml s =
  string_of_xmlm (Sxmlm.xmlm_of_sexp s)

(* (* Xmlm-based encoding of SXML into HTML. Works very well but is too smart 
      and escapes things, when we'd prefer to be able to pass them in raw. *)
let html_of_sxml s =
  let buf = Buffer.create 1024 in
  let out = Xmlm.make_output ~indent:None ~decl:false (`Buffer buf) in
  let id x = x in
  let () = begin
    Buffer.add_string buf "<!DOCTYPE html>\n" ;
    Xmlm.output_doc_tree id out (None, Sxmlm.xmlm_of_sexp s)
  end in
  Buffer.contents buf
*)

let string_of_char = String.make 1
let rec string_of_atom (x : PpxSexp.sexp) : string =
  match x with
  | `Char x -> string_of_char x
  | `Float x -> x
  | `Symbol x -> x
  | `Int x -> string_of_int x
  | `Int32 x -> Int32.to_string x
  | `Int64 x -> Int64.to_string x
  | `Nativeint x -> Nativeint.to_string x
  | `String x -> x
  | `Bool true -> "true"
  | `Bool false -> "false"
  | `List _ -> invalid_arg ("Expected atom, received list: " ^ (string_of_sexp x))
and string_of_sexp x =
  match x with
  | `String x -> "\"" ^ x ^ "\""
  | `List xs -> "(" ^ (String.concat " " (List.map string_of_sexp xs)) ^ ")"
  | _ -> string_of_atom x

let slash_re = Re.compile (
    let open Re in
    char '/'
  )

let parts_of_path = 
  (* pos:1 to skip the beginning / . *)
  Re.split ~pos:1 slash_re 
