# based on Ubuntu Xenial
FROM ubuntu:14.04

# Remove system messages
ENV DEBIAN_FRONTEND noninteractive

# Add ninja directory into PATH
ENV PATH /ninja:$PATH

# Install all dependencies. Work with apt and clean trash-files
RUN apt-get -y update && \
    apt-get -y upgrade && \
    apt-get install -y --no-install-recommends python3.5 python3-pip \
    gcc \
    libcunit1 libcunit1-dev \
    libucl1 libucl-dev \
    libncurses5 libncurses5-dev \
    libncursesw5 libncursesw5-dev \
    wget unzip && \
    rm -rf /var/lib/apt/lists/*

# Get meson minimal support ninja build
RUN mkdir ninja && \
    wget https://github.com/ninja-build/ninja/releases/download/v1.8.2/ninja-linux.zip && \
    unzip -q ninja-linux.zip -d /ninja && \
    rm -f ninja-linux.zip

# Install meson and clean pip cache 
RUN sudo python3.5 -m pip install --upgrade pip && \
    sudo python3.5 -m pip install meson && \
    rm -rf /root/.cache/pip
