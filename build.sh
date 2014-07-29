#!/bin/bash

echo ""
echo "build test program..."
echo ""

./FoBiS.py build -s ./src -compiler gnu -o ./circle_illusion

echo ""
echo "build documentation..."
echo ""

robodoc --src ./src --doc ./doc --internal --multidoc --html --ignore_case_when_linking --syntaxcolors --source_line_numbers --index --tabsize 4 --documenttitle FGIF --sections
