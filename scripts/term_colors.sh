#!/usr/bin/env sh

printf "System colors:\n"
for i in `seq 0 7`; do
    printf "\33[4%sm  " "$i"
done
printf "\33[0m\n"
for i in `seq 0 7`; do
    printf "\33[10%sm  " "$i"
done
printf "\33[0m\n\n"


printf "256-colors: 6x6x6 Color cube:\n"
cat <<EOF

        _ -> Green
    _ -
 - + - - - - > Blue
   |
   |
   |
   v
  Red
EOF

l=16
for i in `seq 0 5`; do
    for j in `seq 0 5`; do
	for k in `seq 0 5`; do
	    printf "\33[48;5;%sm  " "$l"
	    : $((l = l + 1))
	done
	printf "\33[0m "
    done
    printf "\33[0m\n"
done
printf "\33[0m\n"


printf "256-colors: Greyscale ramp:\n"
for i in `seq 232 255`; do
    printf "\33[48;5;%sm  " "$i"
done
printf "\33[0m\n\n"

printf "256-colors: Red ramp:\n"
for i in `seq 16 36 196`; do
    printf "\33[48;5;%sm  " "$i"
done
printf "\33[0m\n\n"

printf "256-colors: Green ramp:\n"
for i in `seq 16 6 46`; do
    printf "\33[48;5;%sm  " "$i"
done
printf "\33[0m\n\n"

printf "256-Colors: Blue ramp:\n"
for i in `seq 16 1 21`; do
    printf "\33[48;5;%sm  " "$i"
done
printf "\33[0m\n\n"


printf "\n88-colors: 4x4x4 Color cube:\n"
l=16
for i in `seq 0 3`; do
    for j in `seq 0 3`; do
	for k in `seq 0 3`; do
	    printf "\33[48;5;%sm  " "$l"
	    : $((l = l + 1))
	done
	printf "\33[0m "
    done
    printf "\33[0m\n"
done
printf "\33[0m\n"


printf "88-colors: Greyscale ramp:\n"
for i in `seq 80 88`; do
    printf "\33[48;5;%sm  " "$i"
done
printf "\33[0m\n\n"

printf "88-colors: Red ramp:\n"
for i in `seq 16 16 79`; do
    printf "\33[48;5;%sm  " "$i"
done
printf "\33[0m\n\n"

printf "88-colors: Green ramp:\n"
for i in `seq 16 4 31`; do
    printf "\33[48;5;%sm  " "$i"
done
printf "\33[0m\n\n"

printf "88-Colors: Blue ramp:\n"
for i in `seq 16 1 19`; do
    printf "\33[48;5;%sm  " "$i"
done
printf "\33[0m\n\n"


printf "Truecolor Test:\n"
str="/"
for col in `seq 0 76`; do
    r=$((255 - $col * 255 / 76))
    g=$(($col * 510 / 76))
    b=$(($col * 255 / 76))
    if [ $g -gt 255 ]; then
        g=$((510 - $g))
    fi
    printf "\033[48;2;%d;%d;%dm" $r $g $b
    printf "\033[38;2;%d;%d;%dm" $((255 - $r)) $((255 - $g)) $((255 - $b))
    printf "%s\033[0m" $str
    if [ $str = "\\" ]; then
        str="/"
    else
        str="\\"
    fi
done
printf "\n";


lerp()
{
    x=$1
    x0=${2-0}
    y0=${3-0}
    x1=${4-0}
    y1=${5-0}
    bc --mathlib --standard <<EOF
(($x - $x0) / ($y0 - $x0)) * ($y1 - $x1) + $x1
EOF
}

rgb2term()
{
    r=${1-0}
    g=${2-0}
    b=${3-0}
    fmt="36 * (${r}/255 * 5) + 6 * ((${g}/255) * 5) + ((${b}/255) * 5) + 16\n"
    printf "%0.f\n" `printf "$fmt" | bc --mathlib --standard`
}

# printf "%s\n" "$(lerp 0.5 0 1 0 5)"
