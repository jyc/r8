let links = [
  ("/", "Home");
  ("/form", "Form")
]

let base ?(head=`List []) ~links body =
  [%sfxp
    '\'',
    (html ((@) (lang "en"))
       (head
          (title "Welcome")
          (link ((@) (rel "stylesheet") (type' "style/css") (href "/static/normalize.css")))
          (link ((@) (rel "stylesheet") (type' "style/css") (href "/static/style.css")))
          [%sp head])
       (body
          (header
             (span ((@) (id "title")) "CS 3110")
             [%spls
               let f (url, name) = 
                 [%sexp (a ((@) (href [%string url])) [%string name])]
               in List.map f links])
          [%sp body]))]

let index dt () = 
  [%sfxp
    '\'', 
    (h1 "Hello, CS 3110 MW 3:35-4:25!")
    (p "Welcome to my website.")
    (p "Click " (a ((@) (href "/form")) "here") " to apply.")
    (p (i "Rendered in " [%string Printf.sprintf "%.3f" (dt *. 1000.)] " ms."))]
  |> base ~links

let success name message =
  [%sfxp
    '\'',
    (h1 "Thanks for your application!")
    (h2 "Name")
    (p [%string name])
    (h2 "Message")
    (p [%string message])]
  |> base ~links

let form () =
  [%sfxp
    '\'',
    (h1 "Application form")
    (p
       "Thanks for choosing to apply. Please fill out this form honestly.")
    (form ((@) (method' "POST"))
       (label
          (h2 "Name")
          (input ((@) (type' "text") (name "name"))))
       (label
          (h2 "Message")
          (textarea ((@) (rows "8")) ""))
       [br]
       (input ((@) (type' "submit") (style "margin-top: 1.125em") (value "Submit"))))]
  |> base ~links
