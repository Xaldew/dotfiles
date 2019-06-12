#!/usr/bin/env sh
# A small script to checkout everything related to llvm.

cd $objects_dir
if [ -d $objects_dir/llvm-project ]; then
    cd $objects_dir/llvm-project
    git pull
else
    git clone https://github.com/llvm/llvm-project.git
fi

# Compile llvm.
cd $objects_dir/llvm-project
mkdir -p build
cd build
cmake -G "Unix Makefiles" \
      -DCMAKE_C_COMPILER=${CC:-gcc} \
      -DCMAKE_CXX_COMPILER=${CXX:-g++} \
      -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra;compiler-rt;lld;polly" \
      -DCMAKE_INSTALL_PREFIX=$local_prefix_dir \
      -DCMAKE_BUILD_TYPE=Release \
      ../llvm/
make -j2
make install
