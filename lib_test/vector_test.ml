open Kaputt.Abbreviations
let (|>) x f = f x

let dist x y = abs_float (x -. y)

let non_empty_token =
  let open Gen in
  list (make_int 1 5) (string (make_int 1 5) alphanum)

module Tests (V : Vector.Vector) = struct
  let make_name s = s ^ " - " ^ V.name

  let () = 
    Test.add_random_test
      ~title:(make_name "cos theta is 1")
      ~nb_runs:100
      non_empty_token
      (fun l -> let v = V.of_list l in V.cos_theta v v -. 1.0)
      [Spec.always ==> Spec.is_zero_float_eps 0.01]

  let () = 
    Test.add_random_test
      ~title:(make_name "norm matches inner product")
      ~nb_runs:100
      non_empty_token
      (fun l -> let v = V.of_list l in (V.norm v, sqrt (V.inner_product v v)))
      [
        Spec.always ==> (fun (v1,v2) -> (dist v1 v2) < 0.001)
      ]

  let () = 
    Test.add_simple_test ~title:(make_name "norm of empty is 0")
      (fun () ->
         let norm = [] |> V.of_list |> V.norm in
         Assert.equal_float ~eps:0.01 norm 0.0)

  let () = 
    Test.add_simple_test ~title:(make_name "norm correct 1")
      (fun () ->
         let norm = ["one";"two"] |> V.of_list |> V.norm in
         Assert.equal_float norm (sqrt 2.0))

  let () = 
    Test.add_simple_test ~title:(make_name "norm is correct 2")
      (fun () ->
         let norm = ["a";"b";"a"] |> V.of_list |> V.norm in
         Assert.equal_float norm (sqrt ((2.0 *. 2.0) +. 1.0)))

  let () =
    Test.add_simple_test ~title:(make_name "ip is correct 1")
      (fun () -> 
         let v = [ "tU_N"; "a"; "4"; "4"; ] |> V.of_list in
         Assert.equal_float 6.0 (V.inner_product v v) )

end

let () = 
  let module M = Tests(Vector.Hash_vector) in
  (*let module M = Tests(Vector.Sorted_array) in*)
  let module N = Tests(Vector.Sorted_list) in ()

let () = Test.launch_tests ()
