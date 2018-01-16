#!/usr/bin/env sh
# Adds useful coloration and svn/git information to the prompt.

export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1_SHOWSTASHSTATE=true
export VIRTUAL_ENV_DISABLE_PROMPT=true
source_if $local_prefix_dir/bin/git-prompt.sh
source_if $dotfiles_dir/scripts/setup_terminal_colors.sh

sh_fg_rgb()
{
    printf "\[\33[38;%sm\]" `rgb $1 $2 $3`
}

sh_bg_rgb()
{
    printf "\[\33[48;%sm\]" `rgb $1 $2 $3`
}

sh_bold()
{
    printf "\[\33[1m\]"
}

sh_underline()
{
    printf "\[\33[4m\]"
}

sh_reverse_video()
{
    printf "\[\33[7m\]"
}

sh_color_off()
{
    printf "\[\33[0m\]"
}


bzr_prompt_info()
{
    bzr_cb=$(bzr nick 2> /dev/null)
    if [ $? -eq 0 ]; then
        bzr_cb=$(printf $bzr_db | \
                     grep -v "ERROR" | \
		     cut -d ":" -f2 | \
		     awk -F / '{print " (bzr::"$1")"}')
        if [ -n "$bzr_cb" ]; then
	    bzr_dirty=""
	    [ -n "`bzr status`" ] && bzr_dirty="$(sh_fg_rgb 155 0 0) *"
	    printf "%s" "$bzr_cb$bzr_dirty"
        fi
    fi
}

git_prompt_info()
{
    __git_ps1 " (git::%s)" 2> /dev/null
}

svn_prompt_info()
{
    info=$(svn info 2>/dev/null)
    if [ $? -eq 0 ]; then
        svn_rev=$(printf "%s" "${info}" | sed -ne 's#^Revision: ##p' | awk '{print $1}')
        svn_root=$(printf "%s" "$info}" | sed -ne 's#^Repository Root: ##p')
        svn_url=$(printf "%s" "${info}" | sed -ne 's#^URL: ##p')
        svn_branch=$(printf "%s" "${svn_url}" |
			 sed -e 's#^'"${svn_root}"'##g' |
			 sed "s|^/branches/||" |
			 awk '{print $1}' |
			 sed -e 's|\([^/]\+\)/\?.*|\1|')
        if [ "${svn_branch#*trunk}" != "${svn_branch}" ]; then
    	    svn_branch=""
        else
	    svn_branch="::${svn_branch}"
        fi
        if [ -n "${svn_rev}" ]; then
	    printf " (svn::%s%s)" $svn_rev "$svn_branch"
        fi
    fi
}

hg_prompt_info()
{
    hg_st=$(hg prompt "(hg::{branch}{::{bookmark}}{status})" 2> /dev/null)
    if [ $? -eq 0 ]; then
	printf " %s" $hg_st
    fi
}

scm_status()
{
    local on_nfs=$(stat -f -L --format="%T" . 2>/dev/null)
    if [ "$on_nfs" != 'nfs' ]; then
	git=$(git_prompt_info)
	svn=$(svn_prompt_info)
	bzr=$(bzr_prompt_info)
	hg=$(hg_prompt_info)
	printf "$(sh_fg_rgb 0 155 0)%s$(sh_color_off)" "${git}${svn}${bzr}${hg}"
    fi
}


scm_light_status()
{
    local on_nfs=$(stat -f -L --format="%T" . 2>/dev/null)
    if [ "$on_nfs" != 'nfs' ]; then
	git=$(git_prompt_info)
	printf "$(sh_fg_rgb 0 155 0)%s$(sh_color_off)" "${git}"
    fi
}


# Display the final symbol of the prompt.
cmd_status()
{
    is_root=$(test $(id --user) -eq 0)
    status=${1:?"No status value given."}
    sym_succ=${2:-"$(sh_fg_rgb   0 155 155)\$$(sh_color_off)"}
    sym_fail=${3:-"$(sh_fg_rgb 155   0   0)\$$(sh_color_off)"}
    root_succ=${4-"$(sh_fg_rgb 155  35   0)#$(sh_color_off)"}
    root_succ=${5-"$(sh_fg_rgb   0 155   0)#$(sh_color_off)"}
    case ${is_root}${status} in
        00) printf "${sym_succ}"; break;;
        01) printf "${sym_fail}"; break;;
        10) printf "${root_succ}"; break;;
        11) printf "${root_fail}"; break;;
    esac
}

# Count the number of running jobs and display them on the prompt.
job_count()
{
    stopped=$(jobs -sp | wc -l)
    running=$(jobs -rp | wc -l)
    ((running+stopped)) &&
	printf "($(sh_fg_rgb 0 100 0)${running}r$(sh_color_off)/" &&
	printf "$(sh_fg_rgb 155 40 0)${stopped}s$(sh_color_off))"
}

# Add virtual environment information to the prompt.
virtualenv_info()
{
    # Get the varible value, if it exists.
    if [ -n "$VIRTUAL_ENV" ]; then
        # Strip out the path and just leave the environment name.
        venv="${VIRTUAL_ENV##*/}"
    else
        # In case you don't have one activated.
        venv=""
    fi
    [ -n "$venv" ] && printf "{$venv} "
}

# Reduce the working directory to around 20 characters.
_dir_chomp ()
{
    local IFS=/
    local c=1
    local n=1
    local d=""
    local p=(${1/#$HOME/\~})
    local r=${p[*]}
    local s=${#r}
    while (( s > $2 && c < ${#p[*]} - 1 ))
    do
        d=${p[c]}
        n=1 ; [[ "$d" = .* ]] && n=2
        : $(( s -= ${#d} - n ))
        p[c++]=${d:0:n}
    done
    echo "${p[*]}"
}


heavy_prompt()
{
    last_status=$?
    PS1="["
    PS1+="$(sh_fg_rgb  65 135  65)$(virtualenv_info)$(sh_color_off)"
    PS1+="$(sh_fg_rgb 135 135 135)\u$(sh_color_off)"
    PS1+="$(sh_fg_rgb 220 220 220)@$(sh_color_off)"
    PS1+="$(sh_fg_rgb 120 120 120)\h$(sh_color_off)"
    PS1+="$(job_count) "
    PS1+=$(_dir_chomp "$PWD" 20)
    PS1+=$(scm_status)
    PS1+="]"
    PS1+="$(cmd_status ${last_status}) "
}


light_prompt()
{
    last_status=$?
    PS1="["
    PS1+="$(sh_fg_rgb  65 135  65)$(virtualenv_info)$(sh_color_off)"
    PS1+="$(sh_fg_rgb 135 135 135)\u$(sh_color_off)"
    PS1+="$(sh_fg_rgb 220 220 220)@$(sh_color_off)"
    PS1+="$(sh_fg_rgb 120 120 120)\h$(sh_color_off)"
    PS1+=" "
    PS1+=$(_dir_chomp "$PWD" 20)
    PS1+=$(scm_light_status)
    PS1+="]"
    PS1+="$(cmd_status ${last_status}) "
}

multiline_prompt()
{
    last_status=$?
    PS1="┌─["
    PS1="\n"
    PS1="└─"
    PS1=$(cmd_status ${last_status} ">>>")
}
