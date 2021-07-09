#!/usr/bin/env sh
# Collection of common utilities used during dotfiles installation.


file_contains_line()
{
    line=${1:?"Missing string to search for."}
    file=${2:?"Missing file to search in."}
    grep -Fxq -e "${line}" -f ${file}
}


add_line_if_missing()
{
    line=${1:?"Missing string to search for."}
    file=${2:?"Missing file to search in."}
    output=${3:-${file}}
    if ! file_contains_line "${line}" "${file}"; then
        printf "${line}\n" >> ${output}
    fi
}


add_supported_ssh_config()
{
    # Check for duplicates and unsupported options.
    tmp=$(mktemp)
    head=$(mktemp)
    while IFS='' read -r line
    do
        add_line_if_missing "$line" "$HOME/.ssh/config" "$tmp"
        case $(ssh -F $tmp _ 2>&1)
        in
            *"Bad configuration option:"*)
                # Unsupported option remove the line.
                head -n -1 $tmp > $head && cp $head $tmp
                ;;
            *"Name or service not known"*)
                # Supported option.
                ;;
        esac
    done < $dotfiles_dir/configs/ssh_config

    cat $tmp >> $HOME/.ssh/config
    rm -r $tmp $head
}


create_linkfarm()
{
    # Create a recrusive linkfarm.
    local src=${1:?"Missing source directory."}
    local dst=${2:?"Missing destination directory."}
    local f=""
    mkdir --parents ${dst}
    for f in ${src}/* ${src}/.[!.]* ${src}/..?*;
    do
        if [ -d $f ]; then create_linkfarm $f $dst/$(basename $f) ; fi
        if [ -f $f ]; then ln -fs -T $f $dst/$(basename $f)       ; fi
    done
}
