open Core.Std

(* I've made two implementations of a vector representing an entity :
   1. a hashtable counting an occurence of every token. The obvious
      implementation of a sparse vector
   2. a sorted list of tokens because i thought calculating an inner product
      or norm might be faster.*)

(** common interface for a vector type *)
module type Vector = sig
  type t
  val name : string (* for testing *)
  val of_list : string list -> t
  val norm : t -> float
  val inner_product : t -> t -> float
  val cos_theta : t -> t -> float
end

module Default = struct
  (* default implementations whenever a consumer module cannot provide
   * a more specialized efficient impl.*)
  let norm ip t = sqrt (ip t t)

  let cos_theta ip t1 t2 = 
    let (norm1, norm2) = (norm ip t1, norm ip t2) in
    Float.((ip t1 t2) / (norm1 * norm2))

  (** cosine when inner product and norm are different *)
  let cos_theta_2 ip norm t1 t2 = 
    let (norm1, norm2) = (norm t1, norm t2) in
    Float.((ip t1 t2) / (norm1 * norm2))

  (** cosine when norm v = sqrt <v,v>  *)
  let cos_theta ip = cos_theta_2 ip (norm ip)
end

module Hash_vector = struct
  type t = (int ref) String.Table.t with sexp
  let name = "Hash_vector"
  let new_vector () = String.Table.create ~size:10 ()
  let incr_coord v coord = 
    Hashtbl.find_or_add v coord ~default:(fun () -> ref 0) |> incr
  let of_list s = 
    let v = new_vector () in
    s |> List.iter ~f:(incr_coord v); v
  let inner_product t1 t2 = 
    let ip = ref 0 in
    t1 |> Hashtbl.iter ~f:(fun ~key ~data -> 
        match Hashtbl.find t2 key with
        | None -> () | Some v -> ip := (!ip) + (!v * !data));
    !ip |> Float.of_int

  let norm v = 
    let count = ref 0 in
    v |> Hashtbl.iter_vals ~f:(fun v -> count := (!count) + (!v * !v));
    !count |> Float.of_int |> sqrt

  let cos_theta = Default.cos_theta_2 inner_product norm
end

module Sorted_list = struct
  type t = string list with sexp (* sorted! *)
  let name = "Sorted_list"
  let of_list = List.sort ~cmp:compare
  (* get the intersection of 2 sorted lists.*)
  let sorted_intersection l1 l2 =
    let rec loop l1 l2 acc = 
      match l1, l2 with
      | [], rest | rest, [] -> List.rev acc
      | x::xs, y::ys when x = y -> loop xs ys (x::acc)
      | x::xs, y::ys when x < y -> loop xs (y::ys) acc
      | x::xs, y::ys -> loop (x::xs) ys acc
    in loop l1 l2 []
  (* inner product from 2 sorted lists of tokens *)
  let stream_count l ~f = 
    let rec loop current count = function
      | x::xs when x = current -> loop current (succ count) xs
      | x::xs -> ((f count current) |> ignore; loop x 1 xs)
      | [] -> f count current
    in match l with
    | x::xs -> loop x 1 xs
    | [] -> ()

  let inner_product l1 l2 = 
    let ip = ref 0 in
    let l = sorted_intersection l1 l2 in
    stream_count l ~f:(fun count _ -> ip := (!ip) + (count * count));
    !ip |> Float.of_int

  let norm l = 
    let ip = ref 0 in
    stream_count l ~f:(fun count _ -> ip := (!ip) + (count * count));
    !ip |> Float.of_int |> sqrt
  let cos_theta = Default.cos_theta_2 inner_product norm
end

module Sorted_array = struct
  type t = string array with sexp
  let name = "Sorted_array"
  let of_list l = l |> Array.of_list
  let norm a = 
    let ip = ref 0 in
    let len = Array.length a - 1 in
    for i = 0 to len do
      let matches = ref 1 in
      for j = (succ i) to len do
        if a.(i) = a.(j) then incr matches
      done;
      ip := (!ip) + ((!matches) * (!matches))
    done;
    !ip |> Float.of_int |> sqrt

  let inner_product a1 a2 = 
    let ip = ref 0 in
    let (l1,l2) = Array.(length a1 - 1, length a2 - 1) in
    for i = 0 to l1 do
      let matches1 = ref 1 in
      let matches2 = ref 0 in
      for k = (succ i) to l1 do
        if a1.(i) = a1.(k) then incr matches1
      done;
      for j = 0 to l2 do
        if a1.(i) = a2.(j) then incr matches2
      done;
      ip := (!ip) + ((!matches1) * (!matches2))
    done;
    !ip |> Float.of_int
      
  let cos_theta = Default.cos_theta_2 inner_product norm
end

(* just for type checking, these modules are useless. There is actually a
   syntax to do this without binding the extra names but I can't google it *)

let () = 
  let module G = (Sorted_list : Vector) in
  let module J = (Sorted_array : Vector) in 
  let module H = (Hash_vector : Vector) in ()

