#!/bin/bash
set -x
dune build
dune exec ./src/main.exe