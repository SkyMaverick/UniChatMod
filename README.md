## ABOUT
UniChatMod is a messanger (and a platform for creating new messengers) based on the ideas of UniChat v1.45 (simplicity and speed) and Miranda-NG (modularity and portability of the profile).

## DEVELOPMENT
* Now building status (Travis-CI - Linux) [![Build Status](https://travis-ci.org/SkyMaverick/UniChatMod.svg?branch=master)](https://travis-ci.org/SkyMaverick/UniChatMod)
* Now building status (Appveyor-CI - Windows) [![Build status](https://ci.appveyor.com/api/projects/status/mk204nbjx0mm5iec?svg=true)](https://ci.appveyor.com/project/SkyMaverick/unichatmod)
* Now syntax check status (Coverity) [![Coverity Scan Build Status](https://scan.coverity.com/projects/17127/badge.svg)](https://scan.coverity.com/projects/skymaverick-unichatmod)

## Compiling and building

The following instruction assume that commands are executed in UniChatMod repository folder

* Install Python (>3.5), meson, gcc, libcunit1 libcunit1-dev (if enabled tests). As options libucl1, libucl1-dev, libncurses5, libncurses5-dev
* Run shell script **run.py <option>**. Now supported options:
    * **build** - rebuild last configuration (debug or release)
    * **debug** - build debug application
    * **release** - build release application (without debug info, strip and optimization)
    * **clean** - remove build directory and cleanup
    * **test** - run test-bot application if checked option in meson config
    * **log** - view build-system log file (need if build failed)
    * **install / uninstall** - install / uninstall application. Create / remove bundle if option *enable_bundle* checked.
