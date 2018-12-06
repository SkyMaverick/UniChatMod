## ABOUT
UniChatMod is a messanger (and a platform for creating new messengers) based on the ideas of UniChat v1.45 (simplicity and speed) and Miranda-NG (modularity and portability of the profile).

## DEVELOPMENT
Now building status [![Build Status](https://travis-ci.org/SkyMaverick/UniChatMod.svg?branch=master)](https://travis-ci.org/SkyMaverick/UniChatMod)

Now syntax check status (Coverity) [![Coverity Scan Build Status](https://scan.coverity.com/projects/17127/badge.svg)](https://scan.coverity.com/projects/skymaverick-unichatmod)

## Compiling and building

The following instruction assume that commands are executed in UniChatMod repository folder

* Install Python (>3.5), meson, gcc, libcunit1 libcunit1-dev (if enabled tests). As options libucl1, libucl1-dev, libncurses5, libncurses5-dev
* Run shell script **run.py <option>**. Now supported options:
    * ** ** - empty option rebuild meson directory
    * **build** - build application
        * *new* - create new debug build
        * *release* - create new release build
    * **clean** - remove build directory and cleanup
    * **test** - run test-bot application if checked option in meson config
    * **log** - view build-system log file (need if build failed)
    * **install / uninstall** - install / uninstall application. Create / remove bundle if option *enable_bundle* checked.
