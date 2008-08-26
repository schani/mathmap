#!/bin/bash

OUTFILE=/tmp/mathtest_$$.png

run_test () {
    SCRIPT=$1
    REFERENCE=$2
    INPUT_ARGS=$3

    echo "Running $SCRIPT"

    if [ ! -f "$REFERENCE" ] ; then
	echo "Reference file doesn't exist - creating it."
	../mathmap -i -f "$SCRIPT" $INPUT_ARGS "$REFERENCE" >&/dev/null
	if [ ! -f "$REFERENCE" ] ; then
	    echo "Error: MathMap didn't produce an output image."
	    exit 1
	fi
    fi

    rm -f "$OUTFILE"
    ../mathmap -i -f "$SCRIPT" $INPUT_ARGS "$OUTFILE" >&/dev/null
    if [ ! -f "$OUTFILE" ] ; then
	echo "Error: MathMap did not produce an output image."
	exit 1
    fi

    CKSUM=`md5sum "$OUTFILE" | awk '{print $1}'`
    REF_CKSUM=`md5sum "$REFERENCE" | awk '{print $1}'`

    if [ "x$CKSUM" != "x$REF_CKSUM" ] ; then
	echo "Error: Output image $OUTFILE doesn't match reference $REFERENCE."
	exit 1
    fi
}

run_render_test () {
    run_test "$1" "$2" "-s 256x256"
}

run_modify_test () {
    run_test "$1" "$2" "-Din=marlene.png"
}

run_modify_test ../examples/Map/Droste.mm droste.png
run_modify_test ../examples/Map/Mugl.mm mugl.png
run_render_test "../examples/Render/Fancy Mandelbrot.mm" mandelbrot.png
run_render_test Apply.mm apply.png
run_modify_test Circle.mm circle.png
run_modify_test Closure.mm closure.png
run_modify_test Twice.mm twice.png
run_modify_test "../examples/Map/IFS Functional.mm" ifs.png
