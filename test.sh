#!/bin/bash
assert() {
    want="$1"
    input="$2"

    ./target/debug/rcc "$input" > tmp.s
    cc -o tmp tmp.s
    ./tmp
    got="$?"

    if [ "$got" = "$want" ]; then
        echo "$input => $got"
    else
        echo "$input => want $want, got $got"
        exit 1
    fi
}

assert 0 0
assert 42 42
assert 21 "5+20-4"
assert 41 " 12 + 34 - 5 "

echo OK
