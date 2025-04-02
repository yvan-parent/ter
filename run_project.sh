#!/bin/bash
clear
set -x
dune exec -- ./src/main.exe -mod Zarith -file T40 -opti T -time F