#!/bin/bash
ARRAY=()

function append_package()
{
    if [ $1 ]; then
	ARRAY+=($1)
	return 0
    else
	return 1
    fi
}


function install_packages()
{
    for pkg in ${ARRAY[@]}
    do
	echo $pkg
    done
    echo ${ARRAY[*]}
}


append_package ""
append_package "test"
append_package "vim"
append_package "emacs"
append_package "clang"

install_packages

