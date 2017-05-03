#!/usr/bin/env sh

# Set the color support for the terminals.
if [ -n "$force_colors" ]; then

    # Explicitly set the terminal to support 256 in e.g., Emacs.
    export terminal_colors=${force_colors}
    export TERM="xterm-${force_colors}color"

elif [ "$OSTYPE" = "cygwin" ]; then

    [ -z "$COLORTERM" ] || export COLORTERM=mintty-24bit
    export terminal_colors="truecolor"

elif [ -n "$COLORTERM" ]; then

    case "$COLORTERM" in
        *24[Bb][Ii][Tt] | *truecolor )
            export terminal_colors="truecolor"
            ;;
        *256color )
            export terminal_colors="256"
            ;;
        gnome-terminal )
            export terminal_colors="truecolor"
            ;;
        xfce4-terminal )
            export terminal_colors="256"
            ;;
    esac

elif command -v tput 1> /dev/null 2>&1; then

    # We have color support; assume it's compliant with Ecma-48 (ISO/IEC-6429).
    export terminal_colors=$(tput -T$TERM colors 2> /dev/null)
    if [ $? -ne 0 -o -z "$terminal_colors" ]; then
        # Unknown terminal error.
        export terminal_colors=0
    fi

else
    # Assume no colors are available.
    export terminal_colors=0
fi


tc_to_256()
{
    cv=0
    if   [ $1 -ge 212 ]; then
	cv=5
    elif [ $1 -lt 212 -a $1 -ge 171 ]; then
	cv=4
    elif [ $1 -lt 171 -a $1 -ge 128 ]; then
	cv=3
    elif [ $1 -lt 128 -a $1 -ge 85 ]; then
	cv=2
    elif [ $1 -lt 85 -a $1 -ge 31 ]; then
	cv=1
    else
	cv=0
    fi
    printf "$cv"
}

tc_to_88()
{
    cv=0
    if   [ $1 -ge 192 ]; then
	cv=3
    elif [ $1 -lt 192 -a $1 -ge 128 ]; then
	cv=2
    elif [ $1 -lt 128 -a $1 -ge 45 ]; then
	cv=1
    else
	cv=0
    fi
    printf "$cv"
}

gray_256()
{
    if   [ $1 -lt 10 ]; then
	printf "5;16"
    elif [ $1 -ge 255 ]; then
	printf "5;255"
    else
	printf "5;%d" $(( 232 + (24*$1/255) ))
    fi
}

rgb_256()
{
    if [ $1 -eq $2 -a $2 -eq $3 ]; then
	gray_256 $1
    else
	r=$(tc_to_256 $1)
	g=$(tc_to_256 $2)
	b=$(tc_to_256 $3)
	printf "5;%d" $(( $r*36 + $g*6 + $b + 16))
    fi
}

gray_88()
{
    if [ $1 -lt 28 ]; then
	printf "5;16"
    elif [ $1 -ge 255 ]; then
	printf "5;88"
    else
	printf "5;%d" $(( 80 + (8*$1/255) ))
    fi
}

rgb_88()
{
    if [ $1 -eq $2 -a $2 -eq $3 ]; then
	gray_88 $1
    else
	r=$(tc_to_88 $1)
	g=$(tc_to_88 $2)
	b=$(tc_to_88 $3)
	printf "5;%d" $(( $1*16 + $2*4 + $3 + 16))
    fi
}

rgb_16()
{
    :
}

rgb()
{
    if [ $terminal_colors = "truecolor" ]; then
	printf "2;%s;%s;%s" $1 $2 $3
    elif [ $terminal_colors -eq 256 ]; then
	rgb_256 $1 $2 $3
    elif [ $terminal_colors -eq 88 ]; then
	rgb_88 $1 $2 $3
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

# Make the following text bold-face.
bold()
{
    printf "\33[1m"
}

# Make the following text underlined.
underline()
{
    printf "\33[4m"
}

# Make the following text displayed in reverse-video.
reverse_video()
{
    printf "\33[7m"
}

# Turn off any colored text.
color_off()
{
    printf "\33[0m"
}
