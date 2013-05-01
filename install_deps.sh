#!/bin/bash
opam switch 4.00.1
eval `opam config env`
opam update
opam install core sexplib parmap cow kaputt sequence --yes
