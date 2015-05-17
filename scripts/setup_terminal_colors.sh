#!/usr/bin/env sh

# Set the color support for the terminals.
if [ -n "$force_colors" ]; then
    # explicitly set the terminal to support 256 in e.g., Emacs.
    terminal_colors=${force_colors}
    export TERM="xterm-${force_colors}color"
elif command -v tput 1> /dev/null 2>&1 && [ -n "$COLORTERM" ]; then
    export TERM='xterm-256color'
elif command -v tput 1> /dev/null 2>&1; then
    # We have color support; assume it's compliant with Ecma-48 (ISO/IEC-6429).
    terminal_colors=$(tput -T$TERM colors)
else
    # Assume no colors are available.
    terminal_colors=0
fi


gray()
{
    v=${1-0}
}

rgb_256()
{
    v=$1
    cv=0
    if   [ $v -lt 256 -a $v -ge 212 ]; then
	cv=5
    elif [ $v -lt 212 -a $v -ge 171 ]; then
	cv=4
    elif [ $v -lt 171 -a $v -ge 128 ]; then
	cv=3
    elif [ $v -lt 128 -a $v -ge 85 ]; then
	cv=2
    elif [ $v -lt 85 -a $v -ge 31 ]; then
	cv=1
    else
	cv=0
    fi
    printf "$cv"
}

rgb_88()
{
    v=$1
    cv=0
    if   [ $v -lt 256 -a $v -ge 192 ]; then
	cv=3
    elif [ $v -lt 192 -a $v -ge 128 ]; then
	cv=2
    elif [ $v -lt 128 -a $v -ge 45 ]; then
	cv=1
    else
	cv=0
    fi
    printf "$cv"
}

rgb_16()
{
    :
}

rgb()
{
    r=${1-0}
    g=${2-0}
    b=${3-0}

    # All colors equal - use grayscale instead.
    if [ $r -eq $g -a $g -eq $b ]; then
	gray $r
    fi

    if [ $terminal_colors -eq 256 ]; then
	rgb_256
    elif [ $terminal_colors -eq 88 ]; then
	:
    elif [ $terminal_colors -eq 16 ]; then
	:
    elif [ $terminal_colors -eq 8 ]; then
	:
    else
	:
    fi
}
