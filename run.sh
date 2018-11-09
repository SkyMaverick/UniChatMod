#!/bin/sh

current_dir=`pwd`
build_dir="$current_dir/build"
tools_dir="$current_dir/tools"
pkg_dir="$current_dir/pkgs"
pkg_src_dir="$pkg_dir/src_pkg"

msn_ninja_file="$build_dir/build.ninja"
msn_log="$build_dir/meson-logs/meson-log.txt"
msn_test_log="$build_dir/meson-logs/testlog.txt"
msn_pack_dir="$build_dir/meson-dist"

sh_manager="$tools_dir/manager.sh"

info() {
    echo "\033[36;1m$1\033[0m"
}

NINJA() {
    cd $build_dir && ninja && cd $current_dir
}

RM_DIR() {
    if [ -d ${1} ]
    then
        rm -rf ${1}
    fi
}

if [ $# -eq 0 ]
then
    if [ -d $build_dir -a -f $msn_ninja_file ]
    then
        NINJA
    else
        meson $current_dir $build_dir && NINJA
    fi
else
    case $1 in
# ==================================================
# Create new local debug build
# ==================================================
        new)
            info "Create new build in: $build_dir"
            
            RM_DIR $build_dir
            mkdir -p $build_dir
            meson $current_dir $build_dir && NINJA
        ;;
# ==================================================
# Clean local repo
# ==================================================
        clean)
            info "Clean build dir: $build_dir"
            RM_DIR ${build_dir}
        ;;
# ==================================================
# Run test_bot application for unit and functional tests
# ==================================================
        test)
            if [ -d $build_dir ]
            then
                info "Start test appliction in : $build_dir"
                cd $build_dir && ninja test &>/dev/null && cd $current_dir
                if [ -f $msn_test_log ]
                then
                    cat $msn_test_log
                else
                    info "Test report create FAILED"
                fi
            else
                info "Make build application as first step"
            fi
        ;;
# ==================================================
# View meson log after build
# ==================================================
        log)
            info "Log build system in : $build_dir"
            if [ -d $build_dir ]
            then
                if [ -f $msn_log ]
                then
                    cat $msn_log
                fi
            else
                info "Make build application as first step"
            fi
        ;;
# ==================================================
# Create src package with meson functions
# ==================================================
        pkg_src)
            info "Create source package in: $current_dir"
            if [ -d $build_dir ]
            then
                cd $build_dir && ninja dist && cd $build_dir
                if [ -d $msn_pack_dir ]
                then
                    mkdir -p $pkg_src_dir
                    cd $msn_pack_dir && mv *.* "$pkg_dir/src_pkg" && cd $current_dir
                fi
            fi
        ;;
# ==================================================
# Create lazarus GUI as custom target
# ==================================================
        lazgui)
            info "Build interface based on Lazarus in: $build_dir"
            if [ -d $build_dir ]
            then
                cd $build_dir && ninja lazgui && cd $build_dir
            fi
        ;;
# ==================================================
# Install application
# ==================================================
        install)
            info "Install application into: $build_dir"
            if [ -d $build_dir ]
            then
                cd $build_dir && ninja install && cd $current_dir
            fi
        ;;
# ==================================================
# Uninstall application
# ==================================================
        uninstall)
            info "Uninstall application into: $build_dir"
            if [ -d $build_dir ]
            then
                cd $build_dir && ninja uninstall && cd $current_dir
            fi
        ;;
# ==================================================
# Standart / Release / Coverity  travis-ci build
# ==================================================
        travis_daily)
            info "Start travis-ci daily build in: $build_dir"
            RM_DIR ${build_dir}
            if [ -d $tools_dir ]
            then
                ${sh_manager} CREATE_FAST
                ${sh_manager} RUN_DEBUG
                ${sh_manager} CLEANUP
            fi
        ;;
        travis_release)
            info "Start travis-ci release build in: $build_dir"
            RM_DIR ${build_dir}
            if [ -d $tools_dir ]
            then
                ${sh_manager} CREATE
                ${sh_manager} RUN_RELEASE
                ${sh_manager} CLEANUP
            fi
        ;;
        travis_coverity)
            info "Start travis-ci coverity check build in: $build_dir"
            RM_DIR ${build_dir}
            if [ -d $tools_dir ]
            then
                ${sh_manager} CREATE
                ${sh_manager} RUN_COVERITY
                ${sh_manager} CLEANUP
            fi
        ;;

# ==================================================
# Update Docker Hub image for fast travis debug build
# ==================================================
        docker_hub)
            info "Update docker image on DockerHub (signin if need)"
            if [ -d $tools_dir ]
            then
                ${sh_manager} UPDATE_DH
            fi
        ;;
# ==================================================
# Default build options
# ==================================================
        build)
            info "start custom build (${2}) in $current_dir to $build_dir"
            if [ -d $build_dir ]
            then
                meson $current_dir $build_dir --buildtype="$2" && NINJA
            else
                mkdir -p $build_dir
                meson $current_dir $build_dir --buildtype="$2" && NINJA
            fi
        ;;
# ==================================================
# Unknow keyword
# ==================================================
        *)
            info "I don't know this command: ${1}"
            exit 1
        ;;
    esac
fi
