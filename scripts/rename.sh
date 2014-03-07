#!/usr/bin/env bash
# Rename euler files.

i=4
for file in eulers*.py;
do
    i=$(($i+1))
    printf -v outf "euler%03d.py" $i
    echo moving: $file to $outf
    mv $file $outf
done

# Useful to add a bunch of rules to cmake:
# for f in euler*.py;
# do
#     echo add_python_target(${::-3} $f) >> CMakeLists.txt;
# done
