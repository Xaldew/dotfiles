#!/usr/bin/env bash
# Install Quicklisp and other Lisp packages given that SBCL is installed.

command -v sbcl || { printf "SBCL not installed." ; exit 1; }
tmp=$(mktemp -d ql.XXXXXX)
function cleanup()
{
    rm -r ${tmp}
}
trap cleanup EXIT

pushd ${tmp}
wget --quiet https://beta.quicklisp.org/quicklisp.lisp
wget --quiet https://beta.quicklisp.org/quicklisp.lisp.asc

# This requires the public key to be installed in the keyring to work.
# gpg --verify quicklisp.lisp.asc quicklisp.lisp || { exit 1; }

sbcl \
    --no-sysinit \
    --no-userinit \
    --load quicklisp.lisp \
    --eval "(quicklisp-quickstart:install :path \"${XDG_CONFIG_HOME}/quicklisp\")" \
    --quit
sbcl \
    --load "${XDG_CONFIG_HOME}/quicklisp/setup.lisp" \
    --eval '(ql:quickload :quicklisp-slime-helper)' \
    --quit
popd
