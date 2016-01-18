xinput list | \
    grep -Po 'id=\K\d+(?=.*slave\s*keyboard)' | \
    xargs -P0 -n1 xinput test | \
    awk 'BEGIN { while (("xmodmap -pke" | getline) > 0) k[$2]=$4 } { print strftime("%c: ") $0 "[" k[$NF] "]" }'
