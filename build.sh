#!/bin/bash

#
#  Build the test program using the gif module.
#
#  Requires:
#    -- FoBiS -- https://github.com/szaghi/FoBiS
#    -- ROBODoc -- http://rfsber.home.xs4all.nl/Robo/
#

echo ""
echo "build test program..."
echo ""

#./FoBiS.py build -s ./src -compiler gnu -o ./circle_illusion
./FoBiS.py build -s ./src -compiler gnu -cflags '-c -O3 -fopenmp' -lflags ' -fopenmp '

#echo ""
#echo "build documentation..."
#echo ""

robodoc --src ./src --doc ./doc --internal --multidoc --html --ignore_case_when_linking --syntaxcolors --source_line_numbers --index --tabsize 4 --documenttitle FGIF --sections
