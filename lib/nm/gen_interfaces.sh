#!/bin/sh

for idesc in $(ls *.xml); do
	obus-gen-interface $idesc
done
