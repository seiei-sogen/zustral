#!/usr/bin/env bash
# Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
# Modified in the Zustral project, a fork of Austral.
# See LICENSE file for details.
#
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

set -euxo pipefail

dune build

function compile() {
    ./zustral compile \
        ./standard/src/Buffer.zui,./standard/src/Buffer.zum \
        ./standard/src/String.zui,./standard/src/String.zum \
        ./standard/src/StringBuilder.zui,./standard/src/StringBuilder.zum \
        ./standard/src/IO/IO.zui,./standard/src/IO/IO.zum \
        ./standard/src/IO/Terminal.zui,./standard/src/IO/Terminal.zum \
        $1/$2.zui,$1/$2.zum \
        --entrypoint=Example.$2:main --output=testbin

    if [ $# -eq 4 ] 
    then
        echo -n -e $4 | ./testbin > actual.txt
    else
        ./testbin > actual.txt
    fi

    echo -n -e "$3" > expected.txt
    diff actual.txt expected.txt
    rm testbin
    rm actual.txt
    rm expected.txt
}

compile examples/ffi FFI "aHello, world!\n"
compile examples/fib Fibonacci ""
compile examples/generic-record GenericRecord ""
compile examples/generic-union GenericUnion ""
compile examples/haversine Haversine ""
compile examples/hello-world HelloWorld "Hello, world!\n"
compile examples/identity Identity ""
compile examples/memory Memory ""
compile examples/named-argument NamedArgument ""
compile examples/record Record ""
compile examples/union Union ""
compile examples/greet Greet "Hello, Santa!\n" "Santa"
