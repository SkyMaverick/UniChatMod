#!/bin/sh

work_dir=`pwd`
src_dir=$work_dir/svg
tg_parent_dir=$work_dir/out
src_ext=svg
tg_ext=png
tg_res=$1'x'$1
tg_dir=$tg_parent_dir/$tg_res

# 

return_dir() {
    if [ -z $1 ]
    then
        if [ -d $1 ]
        then
            cd $1
        else
            echo "Don't change dir (ret into workdir): $1"
            cd $work_dir
        fi
    else
        cd $work_dir
    fi
}

copy_internal_dir_abs() {
    if [ -d $1 ]
    then
        mkdir -p $2
        cd $1
        find ./ -maxdepth 1 | while IFS= read -r filename; do
            cp -R $filename $2
        done
        return_dir
    fi
}

convert_target_dir_abs() {
    if [ -d $1 ]
    then
        cd $1

        find `pwd` -type f -name "*.$src_ext" | while IFS= read -r filename; do
            new_name=`echo $filename | cut -f 1 -d '.'`
            convert -density 100 -background None $filename -support 0.1 -resize $tg_res $new_name.$tg_ext
        done
        
        return_dir
    fi
}

finish_cleanup_dir_abs() {
    if [ -d $1 ]
    then
        cd $1
        find `pwd` -type f -name "*.$src_ext" -print | xargs rm -f
        return_dir
    fi
}

if [ -d $src_dir ]
then
    copy_internal_dir_abs $src_dir $tg_dir
    convert_target_dir_abs $tg_dir
    finish_cleanup_dir_abs $tg_dir
else
    echo "Don't found source dir: $src_dir"
fi
