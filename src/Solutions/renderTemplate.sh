#!/usr/bin/env bash

#NB You need jinja and jinja-cli for this to work
for i in {1..25}; do
  jinja -D day $i -o Day$i.hs Template.j2
done
