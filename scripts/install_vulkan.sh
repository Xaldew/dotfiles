#!/usr/bin/env bash
# Install the Vulkan development header files.

if [ ! -d $objects_dir/Vulkan-Headers ]; then
    git clone https://github.com/KhronosGroup/Vulkan-Headers.git $objects_dir/Vulkan-Headers
else
    cd $objects_dir/Vulkan-Headers
    git pull
fi

mkdir -p $objects_dir/Vulkan-Headers/build
cd $objects_dir/Vulkan-Headers/build
cmake -DCMAKE_INSTALL_PREFIX=$local_prefix_dir ..
make install

if [ ! -d $objects_dir/glslang ]; then
    git clone https://github.com/KhronosGroup/glslang.git $objects_dir/glslang
else
    cd $objects_dir/glslang
    git pull
fi

mkdir -p $objects_dir/glslang/build
cd $objects_dir/glslang/build
cmake -DCMAKE_INSTALL_PREFIX=$local_prefix_dir -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..
make install
