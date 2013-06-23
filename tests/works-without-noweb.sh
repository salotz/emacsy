#!/bin/bash
# works-without-noweb
set -e; # abort if anything fails
cd ..
make dist
dirname=minimal-cognition-0.01
filename=$dirname.tar.gz
tar xfz $filename
cd $dirname
./configure --enable-noweb=no --enable-latexmk=no

# Make sure it can be built without noweb and cleaned without removing
# anything important.
make
make pdf
make clean
make 
