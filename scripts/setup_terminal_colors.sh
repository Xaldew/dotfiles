#!/usr/bin/env sh

# Set the color support for the terminals.
if [ -n "$force_colors" ]; then
    # explicitly set the terminal to support 256 in e.g., Emacs.
    export terminal_colors=${force_colors}
    export TERM="xterm-${force_colors}color"
elif command -v tput 1> /dev/null 2>&1 &&
	 [ "$COLORTERM" == "gnome-terminal" -o \
			"$COLORTERM" == "xfce4-terminal" ]; then
    export TERM='xterm-256color'
    export terminal_colors=256
elif command -v tput 1> /dev/null 2>&1; then
    # We have color support; assume it's compliant with Ecma-48 (ISO/IEC-6429).
    export terminal_colors=$(tput -T$TERM colors)
else
    # Assume no colors are available.
    export terminal_colors=0
fi



tc_to_256()
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

tc_to_88()
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


gray_256()
{
    v=${1-0}
}


rgb_256()
{
    if [ $1 -eq $2 -a $2 -eq $3 ]; then
	gray_256 $1
    fi
    printf "5;%d\n" $(( $1*36 + $2*6 + $3 + 16))
}


gray_88()
{
    :
}


rgb_88()
{
    if [ $1 -eq $2 -a $2 -eq $3 ]; then
	gray_88 $1
    fi
    printf "5;%d\n" $(( $1*16 + $2*4 + $3 + 16))
}

rgb_16()
{
    :
}

rgb()
{
    if [ $terminal_colors == "truecolor" ]; then
	printf "2;%s;%s;%s" $1 $2 $3
    elif [ $terminal_colors -eq 256 ]; then
	rgb_256 $(tc_to_256 $1) $(tc_to_256 $2) $(tc_to_256 $3)
    elif [ $terminal_colors -eq 88 ]; then
	rgb_88 $(tc_to_88 $1) $(tc_to_88 $2) $(tc_to_88 $3)
    elif [ $terminal_colors -eq 16 ]; then
	: # To-be-implemented.
    elif [ $terminal_colors -eq 8 ]; then
	: # To-be-implemented.
    else
	: # To-be-implemented.
    fi
}

# Set a color in the foreground i.e., the text is colored.
fg_rgb()
{
    printf "\33[38;%sm" `rgb $1 $2 $3`
}

bg_rgb()
{
    printf "\33[48;%sm" `rgb $1 $2 $3`
}
