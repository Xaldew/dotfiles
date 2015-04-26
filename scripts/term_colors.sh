#!/usr/bin/env bash

printf "System colors:\n"
for i in `seq 0 7`; do
    printf "\e[4%sm  " "$i"
done
printf "\e[0m\n"
for i in `seq 0 7`; do
    printf "\e[10%sm  " "$i"
done
printf "\e[0m\n\n"


printf "6x6x6 Color cube\n"
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
	    printf "\e[48;5;%sm  " "$l"
	    l=$((l + 1))
	done
	printf "\e[0m "
    done
    printf "\e[0m\n"
done
printf "\e[0m\n"


printf "Greyscale ramp:\n"
for i in `seq 232 255`; do
    printf "\e[48;5;%sm  " "$i"
done
printf "\e[0m\n\n"


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


printf "%s\n" "$(lerp 0.5 0 1 0 5)"



printf "Red ramp:\n"
for i in `seq 16 36 196`; do
    printf "\e[48;5;%sm  " "$i"
done
printf "\e[0m\n\n"

printf "Green ramp:\n"
for i in `seq 16 6 46`; do
    printf "\e[48;5;%sm  " "$i"
done
printf "\e[0m\n\n"

printf "Blue ramp:\n"
for i in `seq 16 1 21`; do
    printf "\e[48;5;%sm  " "$i"
done
printf "\e[0m\n\n"

printf "Truecolor Test:\n"
for r in `seq 0 64 255`; do
    for g in `seq 0 64 255`; do
	for b in `seq 0 64 255`; do
	    printf "\e[48;2;%s;%s;%sm " "$i" "$j" "$k"
	done
	printf "\n"
    done
done
printf "\e[0m\n\n"
