open Cow
open Core.Std

module List = struct
  (* This is necessary because Cow generates code that relies on the compiler's
   * stdlib. While I use the 3rd party stdlib from janestreet (core). (It's
   * customary to use it since it's better designed and more complete) but now I
   * have to import back a few list functions that were renamed by core*)
  include List
  let mem_assoc = Caml.List.mem_assoc
  let assoc     = Caml.List.assoc
end

module JsonCache = struct
  let cache = String.Table.create ~size:20_000 ()
  let set ~key ~json = 
    (* TODO : remove these checks *)
    match Hashtbl.find cache key with
    | None -> Hashtbl.add_exn cache ~key ~data:json
    | Some s -> if s <> json then failwith "dupe"
  let get ~key = Hashtbl.find_exn cache key
end

module type Jsonable = sig
  type t
  val t_of_json : Json.t -> t
  val json_of_t : t -> Json.t
  val key_of : t -> string
end

(* TODO : get rid of this functor *)
module WithCache (J : Jsonable) = struct
  open J
  let of_json s = 
    let t = s |> Json.of_string |> t_of_json in
    JsonCache.set ~key:(key_of t) ~json:s; t
  let to_json t = JsonCache.get ~key:(key_of t)
end

module Product = struct
  include BS (* FIXME *)
  type t = {
    product_name : string;
    manufacturer : string;
    family : string option;
    model : string;
  } with json, sexp
  let of_json s = s |> Json.of_string |> t_of_json
end

module Listing = struct
  module L = struct
    include BS (* FIXME *)
    type t = {
      title : string;
      manufacturer : string;
      currency : string; (* only needed for 1 example *)
      price : string } with json, sexp
    let key_of { title; manufacturer; price ; currency } =
      title ^ manufacturer ^ price ^ currency
  end
  include WithCache(L) include L
end

open Core.Std

module Strings = struct
  let normalize s = s |> String.lowercase |> String.filter 
                      ~f:( function | '-' -> false | _ -> true)
end

let brain_dead products listings = 
  let open Product in
  products |> List.iter ~f:(fun {product_name=p;_} ->
      let open Listing in
      let matches = 
        List.filter listings ~f:(fun {title;_} -> p = title) in
      if List.length matches > 0 then
        printf "Product %s has %d matches\n" p (List.length matches))

let (product_p, listing_p) = ("./data/products.txt", "./data/listings.txt")

let parse_file f parse = f |> In_channel.read_lines |> List.map ~f:parse

let (products, listings) = 
  (parse_file product_p Product.of_json,
   parse_file listing_p Listing.of_json)

let print_top ~x ~printer things = List.take things x |> List.iter ~f:printer

let () = 
  let open List in
  Printf.printf "Have: %d products and %d listings\n"
    (length products) (length listings);
  brain_dead products listings
