#!/bin/bash

PATH_TARGET=${1}
PROJNAME=$2
VERSION=$3

PWD=`pwd`
ARCH=$(uname -m)

echo Build shell package for arch: $ARCH

SHDIR=$PWD
BINDIR=$SHDIR/$PROJNAME

MAKESELF="${PWD}/makeself.sh"
TARGET=${PATH_TARGET}/${PROJNAME}-linux_${ARCH}.sh

if [ -f "${TARGET}" ] ; then rm -f ${TARGET} ; fi

${MAKESELF} --notemp --xz --copy --sha256 \
        --target /opt/${PROJNAME} \
        ${BINDIR} ${TARGET} \
        "UniChatMod" \
