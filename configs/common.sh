#!/usr/bin/env sh
# Common utilities, aliases and functions usable by all POSIX shells.

source_if()
{
    # Source the given files if they exist.
    for f in "$@"; do
	if [ -e "$f" ]; then
	    . $f
	else
	    return
	fi
    done
}

contains()
{
    # Returns 0 if the specified string contains the specified substring,
    # otherwise returns 1.
    str=${1:?"Not given any strings."}
    substr=${2:?"Missing substring."}
    if test "${str#*$substr}" != "$str"
    then
        return 0    # $substr is in $str
    else
        return 1    # $substr is not in $str
    fi
}

command_exists()
{
    command -v "$1" >/dev/null 2>&1
}


open()
{
    # Open each of the given files with the prefered application.
    for f in "$@"; do
	xdg-open $f
    done
}


my_shell()
{
    # Get your current shell
    ps | grep `echo $$` | awk '{ print $4 }'
}


swap_caps()
{
    # Swap between a Vim friendly Caps == ESC and Emacs Caps == ctrl layout.
    # In practice, this swaps the X keyboard options: ctrl:nocaps and
    # caps:escape.
    current=`setxkbmap -query | awk '$1 ~ /^options/ { print $2 }'`
    if contains "$current" "ctrl:nocaps" ; then
        setxkbmap -option ''
        setxkbmap -option 'caps:escape' \
                  -option 'grp:sclk_toggle' \
                  -option 'grp:rctrl_rshift_toggle' \
                  -option 'grp_led:scroll' \
                  -layout se,us
    elif contains "$current" "caps:escape" || [ -z "$current" ]; then
        setxkbmap -option ''
        setxkbmap -option 'ctrl:nocaps' \
                  -option 'grp:sclk_toggle' \
                  -option 'grp:rctrl_rshift_toggle' \
                  -option 'grp_led:scroll' \
                  -layout se,us
    else
        printf "WARN: Unknown XKB option: %s\n" $current
    fi
}


mktmpfs()
{
    # Create a temporary file system in the given folder.
    path=${1-tmpfs}
    size=${2-512m}
    if [ -d "$path" ]; then
        if [ "`ls -A $path`" ]; then
            printf "Error: Non-empty directory: %s.\n" $path
        else
            sudo mount --types tmpfs --options size=$size,mode=777 $path
        fi
    else
        mkdir --parents $path
        sudo mount --types tmpfs --options size=$size,mode=777 $path
    fi
}


pdf_merge()
{
    # Merge the listed files into a single pdf called merged.pdf.
    gs \
	-dBATCH \
	-dNOPAUSE \
	-dQUIET \
	-sDEVICE=pdfwrite \
	-sOutputFile=merged.pdf \
	"$@"
}


pdf_extract()
{
    # Function to extract a range of pdf-pages using ghostscript.
    # This function uses 3 arguments:
    #    $1 is the input file
    #    $2 is the first page of the range to extract
    #    $3 is the last page of the range to extract
    #    The output file will be named "inputfile_pXX-pYY.pdf"
    input=${1}
    start=${2}
    end=${3}
    shift 3
    gs  "$@" \
	-sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER \
	-dFirstPage=${start} \
	-dLastPage=${end} \
	-sOutputFile=${input%.pdf}_p${2}-p${3}.pdf \
	${input}
}


pdf_gray()
{
    # Function to convert a PDF to another grayscale PDF.
    # this function uses 3 arguments:
    #    $1 is the input pdf file.
    #    The output file will be named "'inputfile'_grayscale.pdf"
    input=${1}
    output=$(basename ${1} .pdf)_grayscale.pdf
    shift 1
    gs  "$@" \
	-sOutputFile=${output} \
	-sDEVICE=pdfwrite \
	-sColorConversionStrategy=Gray \
	-dProcessColorModel=/DeviceGray \
	-dCompatibilityLevel=1.4 \
	-dNOPAUSE \
	-dBATCH \
	${input}
}


extract()
{
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)    tar xjf $1        ;;
            *.tar.gz)     tar xzf $1        ;;
            *.tar.xz)     tar xf $1         ;;
            *.bz2)        bunzip2 $1        ;;
            *.rar)        unrar x $1        ;;
            *.gz)         gunzip $1         ;;
            *.tar)        tar xf $1         ;;
            *.tbz2)       tar xjf $1        ;;
            *.tgz)        tar xzf $1        ;;
            *.zip)        unzip $1          ;;
            *.Z)          uncompress $1     ;;
            *)            printf "'$1' cannot be extracted via extract().\n" ;;
        esac
    else
        printf "'$1' is not a valid file.\n"
    fi
}


dls()
{
    # List directories.
    dirs=`ls -l --color | grep "^d" | awk '{ print $9 }' | tr -d "/"`
    printf "%s\n" $dirs
}


dgrep()
{
    # A recursive, case-insensitive grep that excludes binary files.
    grep -iR "$@" * | grep -v "Binary"
}


dfgrep()
{
    # A recursive, case-insensitive grep that excludes binary files
    # and returns only unique filenames.
    grep -iR "$@" * | grep -v "Binary" | \
        sed 's/:/ /g' | awk '{ print $1 }' | sort | uniq
}


psgrep()
{
    # Grep for processes.
    if [ ! -z "$1" ] ; then
        printf "Grepping for processes matching '$1'...\n"
        ps aux | grep "$1" | grep -v grep
    else
        printf "WARN: Need name to grep for.\n"
    fi
}


unpatch()
{
    # Undo previous patching attempts.
    find . -name "*.orig" -o -name "*.rej"  -type f -exec rm {} \;
    find . -name "b" -type d -exec rm -rf {} \;
}


# Create a simple tree implementation if not already available.
if ! command_exists tree; then
    tree()
    {
	find "$@" -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'
    }
fi


rand_int()
{
    # Output a random signed integer.
    # Optional argument is the number of bytes to use for the number.
    size=${1-4}
    od --address-radix=n --read-bytes=${size} --format=d${size} /dev/urandom | \
        sed -e 's/^[ \t]*//'
}


rand_uint()
{
    # Output a random signed integer.
    # Optional argument is the number of bytes to use for the number.
    size=${1-4}
    od --address-radix=n --read-bytes=${size} --format=u${size} /dev/urandom | \
        sed -e 's/^[ \t]*//'
}


find_dropbox()
{
    # Function to find the dropbox folder.
    if [ -d "$HOME/Dropbox" ]; then
	printf "$HOME/Dropbox\n"
        return 0
    elif [ -f "$HOME/.dropbox/host.db" ]; then
        dir=`tail -n 1 $HOME/.dropbox/host.db 2> /dev/null | base64 -d`
        if [ -d "$dir" ]; then
	    printf "%s\n" $dir
            return 0
        else
            return 1
        fi
    else
        return 1
    fi
}


git_dl()
{
    # Download a single file from a git repository.
    # In most cases it may be more suitable to use wget or curl instead.
    #    $1: The URL to the desired repository.
    #    $2: The file we would like to extract.
    url="$1"
    file="$2"
    base_file=`basename ${file}`
    repo=`basename "${url}" ".git"`
    dir=`dirname $file`

    # Attempt to perform archival extraction from the remote.
    ret=0
    git archive --remote=$url HEAD:$dir $file 2> /dev/null | tar -x 2> /dev/null

    # Archiving retrieval failed.
    # Perform a shallow clone in /tmp and extract from there.
    if [ $? -ne 0 ]; then
	tmp_dir=`mktemp --directory`
	cwd=`pwd`
	cd $tmp_dir
	git clone --depth 1 $url > /dev/null 2> /dev/null && \
	    cd $repo >/dev/null &&
            git show HEAD:$file > $cwd/$base_file 2> /dev/null
	ret=$?
	cd $cwd
	rm -rf $tmp_dir
    fi
    return $ret
}


grep_find()
{
    # Grep for things from files found by find.
    find . -type f -exec grep --color -nH -e "$@" {} +
}


bench()
{
    # Benchmark a command.
    #
    # Runs a command multiple times and outputs the average and standard
    # deviation.
    #
    cnt=$1
    shift 1
    tmp=$(mktemp)
    TIMEFORMAT="%3R"
    for i in $(seq 0 $cnt)
    do
        { time "$@" > /dev/null 2>&1 ; } 2>> $tmp
    done

    awk -f- $tmp <<'EOF'
function abs(v) {return v < 0 ? -v : v}
{
    for (i = 1; i <= NF; i++) {
        sum[i] += $i; sumsq[i] += ($i)^2
    }
}
END {
    for (i = 1; i <= NF; i++) {
        printf "%f +- %f \n", sum[i]/NR, sqrt(abs(sumsq[i] - sum[i]^2 / NR) / NR)
    }
}
EOF
    rm $tmp
}


ffprobe_count_subtitles()
{
    stream=${1:?"No video clip set."}
    ffprobe \
        -loglevel error \
        -show_entries stream=codec_type \
        -print_format default=noprint_wrappers=1:nokey=1 \
        ${stream} | grep "subtitle" | wc --lines
}


add_subtitles()
{
    vid=${1:?"No video clip set."}
    sub=${2:?"No subtitle file set."}
    enc=${3:-UTF-8}
    lang=${4:-eng}
    scodec=${5:-srt}
    sidx=$(ffprobe_count_subtitles ${vid})
    tmp=$(mktemp XXXXXXXX.mkv)
    ffmpeg \
        -y \
        -i ${vid} \
        -sub_charenc ${enc} \
        -i ${sub} \
        -map 0 \
        -map 1 \
        -c copy \
        -codec:s ${scodec} \
        -metadata:s:s:${sidx} language="${lang}" \
        ${tmp}
    # Overwrite if successful.
    if [ $? -eq 0 ]; then
        mv -f ${tmp} ${vid}
    fi
}


add_all_subtitles()
{
    vid=${1:?"No video clip set."}
    dir=${2:?"No subtitle directory given."}
    for sub in ${dir}/*;
    do
        lang=$(basename $sub .srt)
        echo "Adding ${sub} with language ${lang} to movie..."
        add_subtitles ${vid} ${sub} UTF-8 ${lang}
    done
}


## Emacs and emacsclient aliases and utility functions.

# Terminal Emacsclient.
nx_emacs_client()
{
    # Start the Emacs client toward the default Emacs daemon socket.
    # Append a new socket name to the arguments to use a different one.
    emacsclient --tty --socket-name=default --alternate-editor="" "$@"
}
alias em="nx_emacs_client"
alias ema="nx_emacs_client"
alias emac="nx_emacs_client"
alias emas="nx_emacs_client"
alias emasc="nx_emacs_client"
alias emascs="nx_emacs_client"
alias eam="nx_emacs_client"

# Start new non-client nx-Emacs instances.
alias nem="emacs --no-window-system"
alias nema="emacs --no-window-system"
alias neam="emacs --no-window-system"
alias nemac="emacs --no-window-system"
alias nemas="emacs --no-window-system"
alias nemacs="emacs --no-window-system"
alias nemasc="emacs --no-window-system"


# Graphical Emacsclient.
graphical_emacs_client()
{
    # Start the Emacs client toward the default Emacs daemon socket.
    # Append a new socket name to the arguments to use a different one.
    emacsclient --create-frame --socket-name=default --alternate-editor="" "$@" &
}
alias ge="graphical_emacs_client"
alias gem="graphical_emacs_client"
alias gema="graphical_emacs_client"
alias gemac="graphical_emacs_client"
alias gemacs="graphical_emacs_client"


# Start new non-client graphical-Emacs instances.
new_graphical_emacs()
{
    \emacs --maximized --no-desktop "$@" &
}
alias ngem="new_graphical_emacs"
alias ngema="new_graphical_emacs"
alias ngeam="new_graphical_emacs"
alias ngemac="new_graphical_emacs"
alias ngemas="new_graphical_emacs"
alias ngemacs="new_graphical_emacs"
alias ngemasc="new_graphical_emacs"


# Pipe stdout to temporary file and open that in an Emacs buffer.
nx_emacspipe()
{
    tmp=`mktemp` && cat > $tmp && nx_emacs_client $tmp ; rm $tmp
}
alias ep="nx_emacspipe"
alias emp="nx_emacspipe"
graphical_emacspipe()
{
    tmp=`mktemp` && cat > $tmp && graphical_emacs_client $tmp ; rm $tmp
}
alias gep="graphical_emacspipe"
alias gemp="graphical_emacspipe"


# Start a new Emacs daemon.
emacs_daemon()
{
    server=${1:-default}
    shift
    \emacs --daemon=$server "$@"
}
alias emd=emacs_daemon
alias emad=emacs_daemon
alias emacd=emacs_daemon
alias emacsd=emacs_daemon


## Miscellaneous aliases.

# Enable color support of ls and also add handy aliases.
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || \
	    eval "$(dircolors -b)"
    # Add default grep options
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto --exclude-dir={\.svn,\.git,\.repo}'
    alias fgrep='fgrep --color=auto --exclude-dir={\.svn,\.git,\.repo}'
    alias egrep='egrep --color=auto --exclude-dir={\.svn,\.git,\.repo}'
fi

alias sshpw="ssh -o PubKeyAuthentication=No"
alias ssh-lth="ssh dt09gw1@login.student.lth.se"

alias ..='cd ..'
alias ....='cd ../..'
alias ......='cd ../../..'

alias sl=ls
alias ll="ls -lFh"
alias la="ls -Alh"
alias lh="ls -hl"
alias lah="ls -hal"
alias l="ls -CF"

alias rs="rsync --recursive"
alias rsp="rsync --progress --recursive"

alias maek=make
alias mkae=make
alias mkea=make
alias amke=make
alias akem=make
alias amek=make
alias meka=make

alias gcc-defines="gcc -dM -E - < /dev/null"
alias g++-defines="g++ -dM -E - < /dev/null"

alias latexmk="latexmk -pdf"

alias valgrind="valgrind --track-origins=yes --leak-check=full --show-reachable=yes"
alias helgrind="\valgrind --tool=helgrind"
alias qemu-arm="qemu-arm -L /usr/arm-linux-gnueabihf"

alias tgi=git
alias gti=git
alias gi=git
alias gt=git

alias svnst="svn st"
alias snv=svn

alias xfce-shortcuts="xfconf-query -c xfce4-keyboard-shortcuts -l -v | \
cut -d'/' -f4 | awk '{printf \"%-30s\", \$2 ; print \"\\t\" \$1 }' | \
sort | uniq"
