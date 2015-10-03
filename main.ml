open Lwt
open Cohttp_lwt_unix
open Data

let ( << ) = Util.( << )

let pack_string s =
  (Cohttp.Transfer.Fixed (Int64.of_int (String.length s)), `String s)

let render =
  pack_string << Util.html_of_sxml

let return_html status (encoding, body) =
  let mime_type = "text/html" in
  let headers = Cohttp.Header.add_opt None "content-type" mime_type in
  let res = Cohttp.Response.make ~status ~encoding ~headers () in
  return (res, body)

let rec get_param ps k =
  match ps with
  | (k', v) :: ps' ->
    if k' = k then Some (String.concat "," v) else get_param ps' k
  | [] -> None

let rec get_param_exn ps k =
  match get_param ps k with
  | Some x -> return x
  | None -> Lwt.fail_with "No param found."

let main port =
  let db = {Database.records=[]} in
  let callback conn req body = 
    Lwt.catch
      (fun () ->
         let ts = Sys.time () in
         let meth = Cohttp.Request.meth req in
         let uri = Cohttp.Request.uri req in
         let path = Uri.path uri in 
         begin match meth, path |> Util.parts_of_path with
           | `GET, ["form"] ->
             Template.form ()
             |> render 
             |> return_html `OK
           | `POST, ["form"] ->
             body |> Cohttp_lwt_body.to_string >>=
             fun s ->
             let params = Uri.query_of_encoded s in
             let param = get_param_exn params in
             param "name" >>= fun name ->
             param "message" >>= fun message ->
             let open Database in
             let () = db.records <- {Entry.name=name; message=message} :: db.records in
             Template.success name message
             |> render
             |> return_html `OK
           | `GET, ["submitted"] ->
             let open Database in
             Template.submitted db.records
             |> render
             |> return_html `OK
           | `GET, [] ->
             Template.index (Sys.time () -. ts) ()
             |> render
             |> return_html `OK
           | `GET, "static" :: _ ->
             (* Get rid of the leading /. Paths will look like static/... .*)
             let fname = String.sub path 1 (String.length path - 1) in
             Server.respond_file ~fname ()
           | _ ->
             pack_string "Not found"
             |> return_html `Not_found
         end)
      (fun e -> 
         let () =
           Printf.printf "Internal error: %s\n%!" (Printexc.to_string e) 
         in Lwt.fail e)
  in
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

