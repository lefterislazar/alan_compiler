#!/usr/bin/env bash

./alan < $1 > a.ll
llc -o a.s a.ll
clang -o a.out a.s lib.a -no-pie
