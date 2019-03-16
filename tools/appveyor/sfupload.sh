#!/bin/sh

echo Add id_rsa in SSH ...

eval "$(ssh-agent -s)"
ssh-add ${1}/tools/appveyor/id_rsa || exit 1

PKGSDIR=${1}/build/pkgs
SSHOPTS="ssh -o StrictHostKeyChecking=no"

if [ -d ${PKGSDIR} ]
then
    rsync -e "$SSHOPTS" ${PKGSDIR}/*.* skymaverick,unicm@frs.sourceforge.net:/home/frs/project/unicm/daily/windows || exit 1
else
    echo "Don't found packages"
fi
