(* the reason why we put these constants in a separate module is so that
   changing these constants will not require a re-compilation of everything*)
let manufacturer_match = 0.25
let listing_match = 0.2
let ncores = 4
(* i've tried a few different implementation of a sparse vector. a hash table
   one turned out to be the fastest*)
module V = Vector.Hash_vector
