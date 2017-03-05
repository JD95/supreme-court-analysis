#!/bin/bash
for f in *.pdf
do

    file=$(echo $f | cut -d '.' -f 1)
    output="$file.txt"
    gs -q -dNODISPALY -P -dSAFER -dDELAYBIND -dWRITESYSTEMDICT -dSIMPLE ps2ascii.ps $f  -c quit > $output
done
