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


printf "Color cube, 6x6x6:\n"
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


rgb2term()
{
    r=${1-0}
    g=${2-0}
    b=${3-0}
    fmt="36 * (${r}/255 * 5) + 6 * ((${g}/255) * 5) + ((${b}/255) * 5) + 16\n"
    printf "%0.f\n" `printf "$fmt" | bc --mathlib --standard`
}

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
for i in `seq 0 1 255`; do
    printf "\e[48;2;%s;0;0m  " "$i"
done
printf "\e[0m\n\n"
