#!/bin/fish

metaocamlc -c stream.ml -o bin/stream.cmo
metaocamlc -I bin -c example.ml -o bin/example.cmo
