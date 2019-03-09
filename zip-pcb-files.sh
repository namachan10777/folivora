#!/bin/bash

mkdir tmp
for d in ./pcb/*; do
	if [[ ! $d =~ .+\..+ ]]; then
		echo $d
		for f in $d/*.kicad_pcb; do
			cp $f ./tmp/$(basename $d).kicad_pcb
		done
	fi
done

zip pcb.zip ./tmp/*
rm -r ./tmp
