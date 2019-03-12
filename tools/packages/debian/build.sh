#!/bin/bash

PROJNAME=$1
VERSION=$2

PWD=`pwd`
ARCH=$(uname -m)

if [[ "$ARCH" == "i686" ]]; then
    DEB_ARCH=i386
elif [[ "$ARCH" == "x86_64" ]]; then
    DEB_ARCH=amd64
else
    echo This arch don\'t support: $ARCH
    exit -1
fi
echo Build debian package for arch: $DEB_ARCH

DEBDIR=$PWD
APPDIR=$PWD/opt
BINDIR=$APPDIR/$PROJNAME

# gen debian-binary
echo "2.0" >$PWD/debian-binary

# append top sections in control file
sed -i -e "1 s/^/Architecture: $DEB_ARCH\n/;" $DEBDIR/control
sed -i -e "1 s/^/Installed-Size: `du -sb $APPDIR | awk '{print int($1/1024)}'`\n/;" $DEBDIR/control
sed -i -e "1 s/^/Version: $VERSION\n/;" $DEBDIR/control

# gen md5sums
find ./opt -type f | while read i ; do
    md5sum "$i" | sed 's/\.\///g' >>$DEBDIR/md5sums
done

chmod 644 $DEBDIR/control $DEBDIR/md5sums
fakeroot -- tar zcvf ./control.tar.gz ./control ./md5sums
fakeroot -- tar zcvf ./data.tar.gz ./opt

fakeroot -- ar cr $PWD/$PROJNAME-linux_$DEB_ARCH.deb debian-binary ./control.tar.gz ./data.tar.gz
