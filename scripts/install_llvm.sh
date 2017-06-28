#!/usr/bin/env sh
# A small script to checkout everything related to llvm and clang for a
# given release.

cd $objects_dir
release=${1-trunk}

if [ -d $objects_dir/llvm ]; then
    cd $objects_dir/llvm
    svn update
else
    url_base=http://llvm.org/svn/llvm-project/
    if [ "$release" != "trunk" ]; then
        release=tags/$release/final
    fi
    svn checkout $url_base/llvm/$release llvm
    if [ $? -eq 0 ]; then
	(cd llvm/tools &&
                svn checkout $url_base/cfe/$release clang)
        (cd llvm/tools/clang/tools &&
                svn checkout $url_base/clang-tools-extra/$release extra)
	(cd llvm/projects &&
		svn checkout $url_base/compiler-rt/$release compiler-rt)
    else
	echo "Error: Failed to download LLVM."
    fi
fi

# Compile llvm.
cd $objects_dir/llvm
mkdir -p build
cd build
cmake -G "Unix Makefiles" \
      -DCMAKE_C_COMPILER=${CC:-gcc} \
      -DCMAKE_CXX_COMPILER=${CXX:-g++} \
      -DCMAKE_INSTALL_PREFIX=$local_prefix_dir \
      -DCMAKE_BUILD_TYPE=Release ..
make -j2
make install
