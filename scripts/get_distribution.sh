#!/usr/bin/env bash
# Try to figure out which distribution is being used.
# Currently tested on:
#    Ubuntu 13.10
#    Ubuntu 15.05
#    Fedora 20

function get_distribution ()
{
    dist="Unknown"
    release="Unknown"
    code_name="Unknown"

    if [ "$(command -v lsb_release)" != "" ]; then

	# If the lsb_release command exists, use it.
    	content=$(lsb_release -a 2>/dev/null)
    	dist=$(echo "$content" | awk 'NR == 1 { print tolower($3) }')
    	release=$(echo "$content" | awk 'NR == 3 { print tolower($2) }')
    	code_name=$(echo "$content" | awk 'NR == 4 { print tolower($2) }')


    elif [ -r /etc/lsb-release ]; then

	# Test if the lsb-release file exists with info, use it instead.
	source /etc/lsb-release
	dist=$DISTRIB_ID
	release=$DISTRIB_RELEASE
	code_name=$DISTRIB_CODENAME

    elif [ -r /etc/redhat-release ]; then

	# Check if we are on a redhat distribution.
	content=$(cat /etc/redhat-release)
	dist=$(echo "$content" | awk '{ print tolower($1) }')
	release=$(echo "$content" | awk '{ print tolower($3) }')
	code_name=$(echo "$content" | awk '{ print tolower($4) }')
	# Remove parantheses.
	code_name=${code_name:1:-1}

    elif [ -r /etc/os-release ]; then

	# No LSB or normal release files found.
	source /etc/os-release
	dist=$ID
	release=$VERSION_ID
	code_name=$(echo $PRETTY_NAME | awk '{ print tolower($3) }')
	code_name=${code_name:1:-1}

    fi

    echo $dist
    echo $release
    echo $code_name

    return 0
}
