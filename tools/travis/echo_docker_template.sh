#!/bin/sh

DEPENDS_CI_IMAGE="\
python3.5 python3-pip \
gcc wget unzip curl git"

DEPENDS_UCM_APP="\
libcunit1 libcunit1-dev \
libucl1 libucl-dev \
libncurses5 libncurses5-dev \
libncursesw5 libncursesw5-dev"

NETFILE_NINJA="https://github.com/ninja-build/ninja/releases/download/v1.8.2/ninja-linux.zip"

echo   " FROM ubuntu:14.04 "
echo   " ENV DEBIAN_FRONTEND noninteractive "
echo   " ENV PATH /ninja:\$PATH "
echo   " RUN apt-get -y update && \ "
echo   "     apt-get -y upgrade && \ "
echo   "     apt-get install -y --no-install-recommends \ "
echo   "     ${DEPENDS_CI_IMAGE} ${DEPENDS_UCM_APP} && \ "
echo   "     rm -rf /var/lib/apt/lists/* "
echo   " RUN mkdir ninja && \ "
echo   "     wget $NETFILE_NINJA && \ "
echo   "     unzip -q ninja-linux.zip -d /ninja && \ "
echo   "     rm -f ninja-linux.zip "
echo   " RUN sudo python3.5 -m pip install --upgrade pip && \ "
echo   "     sudo python3.5 -m pip install meson && \ "
echo   "     rm -rf /root/.cache/pip "
