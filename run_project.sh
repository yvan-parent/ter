#!/bin/bash
clear
set -x
dune build
dune exec ./src/main.exe