#!/usr/bin/sh

echo "This script is context dependent, you need to run it from /cw5"

cd ./cw05
# build release without any warnings
RUSTFLAGS=-Awarnings cargo build --release
cd ..
# this is where rust stores the final binary
mv ./cw05/target/release/cw05 ./func
chmod u+x ./func
# remove a bunch of rust garbage, since we do not need to build again 
# (this can be hundreds of MB)
rm -r -f ./cw05/target/
