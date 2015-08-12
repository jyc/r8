open Lwt
open Cohttp_lwt_unix

let body_of_string s =
  `String s

let pack_string s =
  (Cohttp.Transfer.Fixed (Int64.of_int (String.length s)), body_of_string s)

let html_of_sxml s =
  let buf = Buffer.create 1024 in
  let out = Xmlm.make_output ~indent:(Some 2) ~decl:false (`Buffer buf) in
  let id x = x in
  let () = begin
    Buffer.add_string buf "<!DOCTYPE html>\n" ;
    Xmlm.output_doc_tree id out (None, Sxmlm.xmlm_of_sexp s)
  end in
  Buffer.contents buf

let render_index () =
  [%sexp
    (html (+ (lang "en"))
       (head
          (title "Hello, world!"))
       (body
          (h1 "Powered by S-expressions.")
          (p "You bet.")))
  ]

let render_hello name =
  [%sexp
    (html (+ (lang "en"))
       (head
          (title "Hello there!"))
       (body
          (h1 [%in `String ("Hello, " ^ name ^ "!")])
          (p "Good to see you again.")))
  ]

let slash_re = Re.compile (
    let open Re in
    char '/'
  )

let main port =
  let callback conn req body = 
    let uri = Cohttp.Request.uri req in
    let (encoding, body) =
      (* pos:1 to skip the beginning / . *)
      match Uri.path uri |> Re.split ~pos:1 slash_re with
      | ["test"] ->
        pack_string "warning! experimental testing procedures in place."
      | ["hello"] ->
        let name = begin match Uri.get_query_param uri "name" with
          | Some x -> x
          | None -> "world"
        end in
        render_hello name |> html_of_sxml |> pack_string
      | ["hello2"; name] ->
        render_hello (Uri.pct_decode name) |> html_of_sxml |> pack_string
      | [] ->
        render_index () |> html_of_sxml |> pack_string
      | _ ->
        pack_string "hello, world!" in
    let mime_type = "text/html" in
    let headers = Cohttp.Header.add_opt None "content-type" mime_type in
    let res = Cohttp.Response.make ~status:`OK ~encoding ~headers () in
    return (res, body) in
  let server = Server.make callback () in
  Server.create ~mode:(`TCP (`Port port)) server

let _ =
  let port = ref 8080 in
  let speclist = [
    ("-port", Arg.Set_int port, " The port to serve on. Defaults to 8080.")
  ] in
  let anon_fun s =
    print_endline s in
  let usage_msg =
    "An example HTTP server using cohttp and Lwt." in
  Arg.parse speclist anon_fun usage_msg ;
  Lwt_main.run (main (!port))

