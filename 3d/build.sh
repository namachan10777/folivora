#!/bin/bash

eval $(opam env)

cd $(dirname $0)

STL=(key)
DXF=(key_electrical key_bottom)

echo "generation scad files..."
rm -rf *.scad
ocaml ./src/deploy.ml
for f in ${STL[@]};do
	echo "compiling "$f".scad to "$f".stl"
	openscad -o $f.stl $f.scad
done
for f in ${DXF[@]};do
	echo "compiling "$f".scad to "$f".dxf"
	openscad -o $f.dxf $f.scad
done
zip 3d.zip *.stl *.dxf
