#!/usr/bin/env sh
# Install the latest version of OpenCV.

url=https://github.com/opencv/opencv.git
if [ ! -d $objects_dir/opencv ]; then
    git clone ${url} $objects_dir/opencv
else
    git -C $objects_dir/opencv pull
fi

mkdir ${objects_dir}/opencv/build/
cd ${objects_dir}/opencv/build/
cmake -G "Unix Makefiles" \
      -DCMAKE_C_COMPILER=${CC:-gcc} \
      -DCMAKE_CXX_COMPILER=${CXX:-g++} \
      -DCMAKE_INSTALL_PREFIX=${local_prefix_dir} \
      -DCMAKE_BUILD_TYPE=Release \
      ..
make -j8
make install
