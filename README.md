Entity Resolution Challenge
===========================

Intro
-----
The general approach that I used to tackle this challenge is called a
Vector Space Search Engine. I first learned about this technique as a
method for breaking image captchas and since then I've always found it
to be neat, easy to implement, and has enough parameters to tweak.

Strategy
---------
First we bucket products and listings by manufacturer. The point of this
bucketings is to make our have #(number of buckets) n^2 loops instead 
of 1 massive n^2. Also it's a natural work load for scatter/gather parallelism.

Listings that cannot be bucketed are just put into every bucket (yucky
but better than putting them all in a big bucket with all the products
because the task work distribution will be bad). 

For each bucket the strategy is as folows:
* Tokenize every field of a product/manufacturer I.e. turn it into 1 big
  list of words.
* Convert list of words into a vector (with some weights for example for
  models and against manufacturers)
* Loop over every and listing and select a corresponding product that maximizes
  cos(theta) = `<product, listing>/(||product||*||listing||)`
  Here I assume that a listing applied to a single product only. 
  (Possibly faulty assumption)


Instructions
------------
This is a little bit over complicated because the ocaml eco system
isn't exactly big but once you get opam working it should be easy.

* `sudo apt-get install ocaml` You need some random version of OCaml to
   compile OPAM
* OPAM, get @ `http://opam.ocamlpro.com/doc/Quick_Install.html`.
* OCaml 4.00.1, install this with opam by doing `opam switch 4.00.1`
* execute `./install_deps.sh` which will install:
* core - a decent std lib for OCaml
* sexplib - pretty print some stuff (was useful for debugging)
* cow - json lib
* kaputt - unit + quickcheck style testing
* parmap - easy parallilzation lib

Running
-------
* `make` to compile and run tests
* `./challenge.native` to run program and see results in ./data/results.json

Configuration
-------------
Tweak the parmaters at `lib/fine_structure.ml`
* `manufacturer_match` - min cosine to pair up manufacturers
* `listing_match` - min cosine to pair up listings
* `ncores` - number of cores, should be automatically detected. either
   way you can play with this to how it changes performance. On my
   machine performance is around 2.5x from 4 cores. This is expected
   because of the serial pre processing steps and over head of
   parallilizing.

Discussion
----------
The strengths of my approach is that's pretty general, relatively easy to make
faster, and is tweakable. It's also very good against false positives if you
are willing to accept false negatives as well.

The weaknesses of the approach is memory hungriness and its hard to
eliminate false negatives without introducing more false positives as
well. Also I was not sure if a listing could apply to multiple products
so I assumed that it could not.


The configuration I'm submitting is probably very prone to false positives.
Bumping `listing_match` to `0.25` would give virtually no false positives IMO.
So if I get an outrageous amount of false positives please tune the parameter
up a little ;)

Further Improvements
--------------------
* The implementation is very ugly. Subjectively I would rate this code
  as my B- code. There is not much incentive to improve it as it's just a
  proof of concept. There's a lot of reliance on mutability where it's not
  necessary that could be eliminated.

* There are tons and tons of intermediate allocations that are not necessary.
  Part of it is a by part of exploratory programming in FP which overuses
  linked lists somewhat. I could easily cut the performance in half by 
  replacing some lists with iterables for example.

