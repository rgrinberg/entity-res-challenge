open Core.Std

let normalize s =
  let s = s |> String.lowercase |> String.filter 
            ~f:( function | '/' | '(' | ')' -> false
                          | _ -> true) in
  if String.is_empty s then None else Some(s)

let tokenize = 
  let make_re chars = String.concat ~sep:"*" chars in
  let re = make_re ["_" ; "-" ; ";" ; "," ; " "] |> Str.regexp in
  fun s -> s |> Str.split re |> List.filter_map ~f:normalize 
