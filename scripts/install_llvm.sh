#!/usr/bin/env sh
# A small script to checkout everything related to llvm and clang for a
# given release.

release=${1-trunk}
cd $objects_dir

if [ -d $objects_dir/llvm ]; then
    cd $objects_dir/llvm
    svn update
else
    svn checkout http://llvm.org/svn/llvm-project/llvm/$release llvm
    if [ $? -eq 0 ]; then
	(cd llvm/tools &&
		svn checkout \
		    http://llvm.org/svn/llvm-project/cfe/$release clang)
	(cd llvm/projects &&
		svn checkout \
		    http://llvm.org/svn/llvm-project/compiler-rt/$release \
		    compiler-rt)
    else
	echo "Error: Failed to download LLVM."
    fi
fi

# Compile llvm.
cd $objects_dir/llvm
mkdir build
cd build
cmake -G "Unix Makefiles" \
      -DCMAKE_INSTALL_PREFIX=$local_prefix_dir \
      -DCMAKE_BUILD_TYPE=Release ..
make -j4
make install
