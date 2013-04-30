
(* the reason why we put these constants in a separate module is so that
   changing these constants will not require a re-compilation of everything*)
let manufacturer_match = 0.25
let listing_match = 0.15
let ncores =
  (* stolen from:
     http://stackoverflow.com/questions/16269393/ \
     how-to-get-the-number-of-cores-on-a-machine-with-ocaml  *)
  let cpu_count () = 
    try match Sys.os_type with 
      | "Win32" -> int_of_string (Sys.getenv "NUMBER_OF_PROCESSORS") 
      | _ ->
        let i = Unix.open_process_in "getconf _NPROCESSORS_ONLN" in
        let close () = ignore (Unix.close_process_in i) in
        try Scanf.fscanf i "%d" (fun n -> close (); n) with e -> close ();
          raise e
    with
    | Not_found | Sys_error _ | Failure _ | Scanf.Scan_failure _ 
    | End_of_file | Unix.Unix_error (_, _, _) -> 1
  in try cpu_count () with _ -> 4

(* i've tried a few different implementation of a sparse vector. a hash table
   one turned out to be the fastest*)
module V = Vector.Hash_vector
