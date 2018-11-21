#!/bin/sh
libname=libutf8proc.a

make -j ${libname} && \
cp ${libname} ${1}
