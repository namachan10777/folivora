#!/usr/bin/bash

rm *.scad
echo "generation scad files..."
utop ./src/deploy.ml
for f in *.scad;do
	echo "compiling " $f
	openscad -o $(basename $f .scad).stl $f
done
