#!/bin/bash

set -e

shopt -s globstar

TARGET=$1

cargo build --release

for rust_file in $TARGET/**/*.rs; do
    echo $rust_file
    ./target/release/dump_tokens "$rust_file" > /dev/null
done
