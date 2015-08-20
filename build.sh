#!/bin/bash

#
#  Build the test program using the gif module.
#
#  Requires:
#    -- FoBiS : https://github.com/szaghi/FoBiS
#    -- FORD  : https://github.com/cmacmackin/ford
#

FORDMD='fgif.md'                     # FORD config file
DOCDIR='./doc/'                      # build directory for documentation
SRCDIR='./src/'                      # library source directory
TESTDIR='./src/tests/'               # tests source directory
BINDIR='./bin/'                      # build directory for example
LIBDIR='./lib/'                      # build directory for library
MODCODE='gif_module.f90' 		     # FGIF module file name
LIBOUT='libfgif.a'                   # name of FGIF library

# compiler settings:
FCOMPILER='gnu'
FCOMPILERFLAGS='-c -O3 -fopenmp'
FLINKERFLAGS=' -fopenmp '

if hash FoBiS.py 2>/dev/null; then

    #build the stand-alone library:
    FoBiS.py build -compiler ${FCOMPILER} -cflags "${FCOMPILERFLAGS}" -lflags "${FLINKERFLAGS}" -dbld ${LIBDIR} -s ${SRCDIR} -dmod ./ -dobj ./ -t ${MODCODE} -o ${LIBOUT} -mklib static -colors 

    #build the test programs:
    FoBiS.py build -compiler ${FCOMPILER} -cflags "${FCOMPILERFLAGS}" -lflags "${FLINKERFLAGS}" -dbld ${BINDIR} -s ${TESTDIR} -i ${LIBDIR} -dmod ./ -dobj ./ -libs ${LIBDIR}${LIBOUT} -colors
    
else
    echo "FoBiS.py not found! Cannot build library. Install using: sudo pip install FoBiS.py"
fi

if hash ford 2>/dev/null; then

    ford ${FORDMD}

else
    echo "Ford not found! Cannot build documentation. Install using: sudo pip install ford"
fi
