#!/usr/bin/env sh
# A script to download and install the latest ffmpeg and all related libraries.

sudo apt-get install --quiet=2 \
     nasm \
     libass-dev \
     libtheora-dev \
     libvorbis-dev


mkdir --parents \
      $objects_dir/ffmpeg \
      $objects_dir/ffmpeg/ffmpeg_build

install_yasm()
{
    # Compile and install yasm.
    cd $objects_dir/ffmpeg
    if [ ! -d yasm ]; then
	git clone git://github.com/yasm/yasm.git
    else
	git -C yasm pull
    fi
    cd $objects_dir/ffmpeg/yasm
    ./autogen.sh
    ./configure \
	--prefix=$objects_dir/ffmpeg_build \
	--bindir=$local_prefix_dir/bin
    make
    make install
    make distclean
}

install_libx264()
{
    # Compile and install libx264.
    cd $objects_dir/ffmpeg
    if [ ! -d libx264 ]; then
	git clone git://git.videolan.org/x264.git
    else
	git -C libx264 pull
    fi
    cd $objects_dir/ffmpeg/x264
    ./configure \
	--prefix=$objects_dir/ffmpeg/ffmpeg_build \
	--enable-static
    make
    make install
    make distclean
}

install_libx265()
{
    # Compile and install libx265 (HEVC)
    cd $objects_dir/ffmpeg
    if [ ! -d libx265 ]; then
	hg clone https://bitbucket.org/multicoreware/x265
    else
	git -C libx265 pull
    fi
    cd $objects_dir/ffmpeg/x265/build/linux
    cmake \
	-DCMAKE_INSTALL_PREFIX=$objects_dir/ffmpeg/ffmpeg_build \
	-DENABLE_SHARED=off \
	../../source
    make
    make install
    make clean
}

install_libvpx()
{
    # Download, compile and install libvpx.
    cd $objects_dir/ffmpeg
    if [ ! -d libvpx ]; then
	git clone git://github.com/webmproject/libvpx.git
    else
	git -C libvpx pull
    fi
    cd libvpx
    ./configure \
	--prefix=$objects_dir/ffmpeg/ffmpeg_build \
	--disable-examples
    make
    make install
    make clean
}

install_libopus()
{
    # Install Opus audio codecs.
    cd $objects_dir/ffmpeg
    if [ ! -d opus ]; then
	git clone git://git.opus-codec.org/opus.git
    else
	git -C opus pull
    fi
    cd opus
    ./autogen.sh
    ./configure \
	--prefix=$objects_dir/ffmpeg/ffmpeg_build \
	--disable-shared
    make
    make install
    make distclean
}

install_libmp3_lame()
{
    # Install mp3 lame audio codec.
    url=http://downloads.sourceforge.net/project/
    url+=lame/lame/3.99/lame-3.99.5.tar.gz
    cd $objects_dir/ffmpeg
    if [ ! -d lame ]; then
	mkdir --parents lame
	wget $url --output-document=lame.tar.gz --quiet
	tar xzvf lame.tar.gz -C lame --strip-components=1
	rm lame.tar.gz
    fi
    cd lame
    ./configure \
	--prefix=$objects_dir/ffmpeg/ffmpeg_build \
	--enable-nasm \
	--disable-shared
    make
    make install
    make distclean
}

install_fdk_aac()
{
    # Install fdk-AAC audio codec.
    cd $objects_dir/ffmpeg
    if [ ! -d fdk-aac ]; then
	git clone git://github.com/mstorsjo/fdk-aac.git
    else
	git -C fdk-aac pull
    fi
    cd fdk-aac
    autoreconf -fiv
    ./configure \
	--prefix=$objects_dir/ffmpeg/ffmpeg_build \
	--disable-shared
    make
    make install
    make distclean
}

# Several packets need yasm, wait for it first.
install_yasm

pids=()
fails=0
install_libx264 &
pids+=($!)
install_libx265 &
pids+=($!)
install_libvpx &
pids+=($!)
install_libopus &
pids+=($!)
install_libmp3_lame &
pids+=($!)
install_fdk_aac &
pids+=($!)

# Wait for everything to finish.
for pid in "${pids[@]}"; do
    wait $pid || let "fails+=1"
done

if [ ! $fails -eq 0 ]; then
   printf "Warning: Failed to compile something."
   exit -1
fi

# Finally, compile and install ffmpeg.
cd $objects_dir/ffmpeg
if [ ! -d ffmpeg_sources ]; then
    git clone git://github.com/FFmpeg/FFmpeg.git ffmpeg_sources
else
    git -C ffmpeg_sources pull
fi
cd $objects_dir/ffmpeg/ffmpeg_sources
PKG_CONFIG_PATH=$objects_dir/ffmpeg/ffmpeg_build/lib/pkgconfig \
	       ./configure \
	       --prefix=$objects_dir/ffmpeg/ffmpeg_build \
	       --bindir=$local_prefix_dir/bin \
	       --mandir=$local_prefix_dir/share/man \
	       --pkg-config-flags="--static" \
	       --extra-cflags=-I$objects_dir/ffmpeg/ffmpeg_build/include \
	       --extra-ldflags=-L$objects_dir/ffmpeg/ffmpeg_build/lib \
	       --enable-static \
	       --disable-shared \
	       --enable-gpl \
	       --enable-nonfree \
	       --enable-libass \
	       --enable-libfdk-aac \
	       --enable-libfreetype \
	       --enable-libmp3lame \
	       --enable-libopus \
	       --enable-libtheora \
	       --enable-libvorbis \
	       --enable-libvpx \
	       --enable-libx264 \
	       --enable-libx265
make
make install
make distclean
hash -r
