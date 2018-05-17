#!/bin/sh

current_dir=`pwd`
build_dir="$current_dir/build"
pkg_dir="$current_dir/pkgs"
pkg_src_dir="$pkg_dir/src_pkg"

msn_ninja_file="$build_dir/build.ninja"
msn_log="$build_dir/meson-logs/meson-log.txt"
msn_test_log="$build_dir/meson-logs/testlog.txt"
msn_pack_dir="$build_dir/meson-dist"

NINJA() {
    cd $build_dir && ninja && cd $current_dir
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
        new)
            if [ -d $build_dir ]
            then
                rm -r $build_dir 
            fi
            mkdir -p $build_dir
            meson $current_dir $build_dir && NINJA
        ;;
        clean)
            if [ -d $build_dir ]
            then
                rm -rf $build_dir
            fi
        ;;
        test)
            if [ -d $build_dir ]
            then
                cd $build_dir && ninja test &>/dev/null && cd $current_dir
                if [ -f $msn_test_log ]
                then
                    cat $msn_test_log
                else
                    echo "Test report create FAILED"
                fi
            else
                echo "Make build application as first step"
            fi
        ;;
        log)
            if [ -d $build_dir ]
            then
                if [ -f $msn_log ]
                then
                    cat $msn_log
                fi
            else
                echo "Make build application as first step"
            fi
        ;;
        pkg_src)
            if [ -d $build_dir ]
            then
                cd $build_dir && ninja dist && cd $build_dir
                if [ -d $msn_pack_dir ]
                then
                    mkdir -p $pkg_src_dir
                    cd $msn_pack_dir && mv *.* "$pkg_dir/src_pkg" && cd $currentdir
                fi
            fi
        ;;
        install)
            if [ -d $build_dir ]
            then
                cd $build_dir && ninja install && cd $current_dir
            fi
        ;;
        uninstall)
            if [ -d $build_dir ]
            then
                cd $build_dir && ninja uninstall && cd $current_dir
            fi
        ;;
        *)
            if [ -d $build_dir ]
            then
                meson $current_dir $build_dir --buildtype="$1" && NINJA
            else
                mkdir -p $build_dir
                meson $current_dir $build_dir --buildtype="$1" && NINJA
            fi
        ;;
    esac
fi
