open Cow
open Core.Std
open Fine_structure


(* make minor collections less frequent since we do allocate a lot of
   intermediate data structures *)
let () = Gc.tune ~minor_heap_size:2_000_000 ()

(* This is necessary because Cow generates code that relies on the compiler's
   stdlib. While I use the 3rd party stdlib from janestreet: core. It's
   customary to use it since it's better designed and more complete but now I
   have to import back a few list functions that were renamed by core*)

module List = struct
  include List
  let mem_assoc = Caml.List.mem_assoc
  let assoc     = Caml.List.assoc

  let group_by l ~f =
    let h = Hashtbl.Poly.create ~size:200 () in
    l |> List.iter ~f:(fun x ->
        let l = Hashtbl.find_or_add h (f x) ~default:(fun () -> ref []) in
        l := x :: (!l));
    h |> Hashtbl.keys |> List.map ~f:(fun k -> (k, !(Hashtbl.find_exn h k)))

  let max_by l ~f = 
    let rec loop current_min rank = function
      | [] -> (current_min, rank)
      | x::xs ->
        let x_rank = f x in
        if x_rank > rank
        then loop x x_rank xs
        else loop current_min rank xs
    in match l with
    | [] -> raise (Invalid_argument "max_by cannot take empty list")
    | x::xs -> loop x (f x) xs
end

type token = string list

module JsonCache = struct
  let cache = String.Table.create ~size:20_000 ()
  let set ~key ~json = 
    (* TODO : remove these checks *)
    match Hashtbl.find cache key with
    | None -> Hashtbl.add_exn cache ~key ~data:json
    | Some s -> if s <> json then failwith "dupe"
  let get ~key = Hashtbl.find_exn cache key
end

module Product = struct
  type t = {
    product_name : string;
    manufacturer : string;
    family : string option;
    model : string;
  } with json, sexp
  let of_json s = s |> Json.of_string |> t_of_json
  let to_vector {product_name; family; model; manufacturer; _} = 
    let family = Option.value family ~default:"" in
    let (t_model, t_family, t_product_name, t_manufacturer) =
      Token.(tokenize model, tokenize family, tokenize product_name,
             tokenize manufacturer) in
    let weights = List.concat [
        t_model        |> List.map ~f:(fun x -> (x, 10));
        t_manufacturer |> List.map ~f:(fun x -> (x, 0))
      ] in 
    V.of_weighted_list (List.concat [t_model; t_family; t_product_name])
      ~weights
end

module Listing = struct
  type t = {
    title : string;
    manufacturer : string;
    currency : string; (* only needed for 1 example *)
    price : string } with json, sexp
  let key_of { title; manufacturer; price ; currency } =
    title ^ manufacturer ^ price ^ currency
  let of_json s = 
    let t = s |> Json.of_string |> t_of_json in
    JsonCache.set ~key:(key_of t) ~json:s; t
  let to_json t = JsonCache.get ~key:(key_of t)
  let to_vector {title;_} = title |> Token.tokenize |> V.of_list
end

module EntityResult = struct
  (* Sorry about the following. There's a bug in the json lib I'm using that's
     preventing me from generating this json properly. Libraries in OCaml are
     sometimes a little sketchy...*)
  let js_result product listings = 
    let b = Buffer.create 1000 in
    Buffer.add_string b "{\"product_name\": \"";
    Buffer.add_string b product.Product.product_name;
    Buffer.add_string b "\", \"listing\": ";
    Buffer.add_char b '[';
    listings
    |> List.map ~f:Listing.to_json 
    |> String.concat ~sep:"," 
    |> Buffer.add_string b;
    Buffer.add_string b "]}"; b |> Buffer.contents
end

let product_bucket = String.Table.create ~size:50 ()
let listing_bucket = String.Table.create ~size:500 ()

let find_bucket h k = !(Hashtbl.find_exn h k)

let (product_p, listing_p) = ("./data/products.txt", "./data/listings.txt")

let parse_file f parse = f |> In_channel.read_lines |> List.map ~f:parse

(* parser with side effect of populating buckets*)
let parser_of ~parse ~bucket ~get_m x =
  let parsed = parse x in 
  let manu = get_m parsed in
  let items = Hashtbl.find_or_add bucket manu~default:(fun () -> ref []) in 
  items := parsed :: !(items); parsed

let (products, listings) = 
  (parse_file product_p (
      parser_of ~parse:Product.of_json ~bucket:product_bucket
        ~get_m:(fun {Product.manufacturer;_} -> manufacturer)),
   parse_file listing_p (
     parser_of ~parse:Listing.of_json ~bucket:listing_bucket
       ~get_m:(fun {Listing.manufacturer;_} -> manufacturer)))

let unify_manufacturers () = 
  let threshold = Fine_structure.manufacturer_match in
  let vectorize l = l |> Token.tokenize |> V.of_list in
  let matched = ref String.Set.empty in
  let listing_manus = Hashtbl.keys listing_bucket in
  let results =
    product_bucket |> Hashtbl.fold ~init:[] ~f:(fun ~key:manup ~data:prod c ->
      let vp = vectorize manup in
      let matches = 
        listing_manus
        |> List.filter_map ~f:(fun manul ->
            if (V.cos_theta (vectorize manul) vp) > threshold
            then begin
              matched := Set.add (!matched) manul;
              Some (manup, manul)
            end
            else None)
      in matches @ c) |> ref in
  let unmatched = Set.diff (String.Set.of_list (listing_manus)) !matched in
  product_bucket |> Hashtbl.keys |> List.iter ~f:(fun manup -> 
      unmatched |> Set.iter ~f:(fun manul ->
          results := (manup, manul) :: (!results)));
      !results

let match_up manup manuls =
  let matches = ref 0 in
  (* collect matches in this hash table *)
  let products_classifier = String.Table.create ~size:50 () in
  (* pre compute vectors for products *)
  let products = find_bucket product_bucket manup 
                 |> List.map ~f:(fun x -> (x, Product.to_vector x)) in
  manuls |> List.iter ~f:(fun manul ->
      (find_bucket listing_bucket manul) |> List.iter ~f:(fun listing ->
          let listing_v = Listing.to_vector listing in
          let ((product,_),cos) =
            List.max_by products ~f:(fun (_,pv) -> V.cos_theta listing_v pv) in
          if cos > Fine_structure.listing_match then begin
            let product_matches =  Hashtbl.find_or_add products_classifier
                product.Product.product_name
                ~default:(fun () -> ref []) in
            product_matches := listing::(!product_matches);
            incr matches;
          end));
  let res = products |> List.filter_map ~f:(fun (product, _) ->
      let product_name = product.Product.product_name in
      (Hashtbl.find products_classifier product_name) 
      |> Option.map ~f:(fun matches ->
          EntityResult.js_result product !matches)) in
  (res, !matches)

let () = 
  let open List in
  let manus = unify_manufacturers () in

  let matches = Parmap.L(manus |> List.group_by ~f:fst)
                |> Parmap.parmap ~ncores
                  (fun (p, pairs) -> let ls = List.map pairs ~f:snd in
                    match_up p ls) in
  printf "In total matched: %d\n"
    (List.fold_left ~init:0 ~f:(fun acc matches -> 
         acc + (snd matches)) matches);
  Out_channel.with_file "./data/results.json" ~f:(fun out ->
      Out_channel.output_char out '[';
      matches |> List.concat_map ~f:fst |> String.concat ~sep:",\n"
      |> Out_channel.output_string out;
      Out_channel.output_char out ']')
