#!/usr/bin/env sh
# A small script to checkout everything related to llvm and clang for a
# given release.

release=${1-trunk}
mkdir -p $HOME/git/installs
cd $HOME/git/installs
svn checkout http://llvm.org/svn/llvm-project/llvm/$release llvm
if [ $? -eq 0 ]; then
    (cd llvm/tools &&
	    svn checkout http://llvm.org/svn/llvm-project/cfe/$release clang)
    (cd llvm/projects &&
	    svn checkout http://llvm.org/svn/llvm-project/compiler-rt/$release \
		compiler-rt)
else
    echo "Error: Failed to download LLVM."
fi
