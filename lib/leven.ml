(* Minimum of three integers. This function is deliberately
 * not polymorphic because (1) we only need to compare integers 
 * and (2) the OCaml compilers do not perform type specialization 
 * for user-defined functions. *)
let minimum (x:int) y z =
  let m' (a:int) b = if a < b then a else b in
  m' (m' x y) z

(* Matrix initialization. *)
let init_matrix n m =
  let init_col = Array.init m in
  Array.init n (function
      | 0 -> init_col (function j -> j)
      | i -> init_col (function 0 -> i | _ -> 0)
    )

(* Computes the Levenshtein distance between two unistring.
 * If you want to run it faster, add the -unsafe option when
 * compiling or use Array.unsafe_* functions (but be carefull 
 * with these well-named unsafe features). *)
let distance_utf8 x y =
  match Array.length x, Array.length y with
  | 0, n -> n
  | m, 0 -> m
  | m, n ->
    let matrix = init_matrix (m + 1) (n + 1) in
    for i = 1 to m do
      let s = matrix.(i) and t = matrix.(i - 1) in
      for j = 1 to n do
        let cost = abs (compare x.(i - 1) y.(j - 1)) in
        s.(j) <- minimum (t.(j) + 1) (s.(j - 1) + 1) (t.(j - 1) + cost)
      done
    done;
    matrix.(m).(n)

let distance x y =
  distance_utf8 (Glib.Utf8.to_unistring x) (Glib.Utf8.to_unistring y)
