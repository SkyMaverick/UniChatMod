#!/bin/sh
./configure CFLAGS=-std=c89 --enable-shared --with-gnu-ld --with-pic && make -j && cp $1 $2
