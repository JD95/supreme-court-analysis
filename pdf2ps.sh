#!/bin/bash

for f in *.pdf; do; pdf2ps $f; done
