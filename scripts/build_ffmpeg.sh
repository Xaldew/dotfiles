#!/usr/bin/env bash
# A script to download and install the latest ffmpeg and all related libraries.

# DEB: sudo apt-get install nasm libsdl1.2-dev

set -e

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
	--prefix=$objects_dir/ffmpeg/ffmpeg_build
    make
    make install
}

install_vorbis()
{
    cd $objects_dir/ffmpeg
    if [ ! -d vorbis ]; then
	git clone https://git.xiph.org/vorbis.git
    else
	git -C vorbis pull
    fi
    cd $objects_dir/ffmpeg/vorbis
    ./autogen.sh
    ./configure \
	--prefix=$objects_dir/ffmpeg/ffmpeg_build \
	--enable-static \
        --disable-shared
    make
    make install
}

install_theora()
{
    cd $objects_dir/ffmpeg
    if [ ! -d theora ]; then
        git clone https://git.xiph.org/theora.git
    else
	git -C theora pull
    fi
    cd $objects_dir/ffmpeg/theora
    ./autogen.sh
    ./configure \
	--prefix=$objects_dir/ffmpeg/ffmpeg_build \
	--enable-static \
        --disable-shared
    make
    make install
}

install_libass()
{
    cd $objects_dir/ffmpeg
    if [ ! -d libass ]; then
        git clone https://github.com/libass/libass.git
    else
	git -C libass pull
    fi
    cd $objects_dir/ffmpeg/libass
    ./autogen.sh
    ./configure \
	--prefix=$objects_dir/ffmpeg/ffmpeg_build \
	--enable-static
    make
    make install
}

install_libx264()
{
    # Compile and install libx264.
    cd $objects_dir/ffmpeg
    if [ ! -d x264 ]; then
	git clone git://git.videolan.org/x264.git
    else
	git -C x264 pull
    fi
    cd $objects_dir/ffmpeg/x264
    ./configure \
	--prefix=$objects_dir/ffmpeg/ffmpeg_build \
	--enable-static
    make
    make install
}

install_libx265()
{
    # Compile and install libx265 (HEVC)
    cd $objects_dir/ffmpeg
    if [ ! -d x265 ]; then
	hg clone https://bitbucket.org/multicoreware/x265
    else
	hg --cwd x265 pull
    fi
    cd $objects_dir/ffmpeg/x265/build/linux
    cmake \
	-DCMAKE_INSTALL_PREFIX=$objects_dir/ffmpeg/ffmpeg_build \
	-DENABLE_SHARED=off \
	../../source
    make
    make install
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
}

install_ogg()
{
    # Install ogg audio container accessor.
    cd $objects_dir/ffmpeg
    if [ ! -d ogg ]; then
	git clone git://git.xiph.org/ogg.git
    else
	git -C ogg pull
    fi
    cd ogg
    ./autogen.sh
    ./configure \
	--prefix=$objects_dir/ffmpeg/ffmpeg_build \
        --enable-static \
	--disable-shared
    make
    make install
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
}

install_libmp3_lame()
{
    # Install mp3 lame audio codec.
    url=http://downloads.sourceforge.net/project/lame/lame/3.99/lame-3.99.5.tar.gz
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
}

# Several packets need yasm, wait for it first.
install_yasm

export PKG_CONFIG_PATH=$objects_dir/ffmpeg/ffmpeg_build/lib/pkgconfig
export PATH=$PATH:$objects_dir/ffmpeg/ffmpeg_build/bin

install_ogg
install_libopus
install_vorbis
install_theora
install_libass
install_libx264
install_libx265
install_libvpx
install_libmp3_lame
install_fdk_aac

# pids=()
# fails=0
# install_vorbis &
# pids+=($!)
# install_theora &
# pids+=($!)
# install_ass &
# pids+=($!)
# install_libx264 &
# pids+=($!)
# install_libx265 &
# pids+=($!)
# install_libvpx &
# pids+=($!)
# install_libopus &
# pids+=($!)
# install_libmp3_lame &
# pids+=($!)
# install_fdk_aac &
# pids+=($!)

# Wait for everything to finish.
# for pid in "${pids[@]}"; do
#     wait $pid || let "fails+=1"
# done

# if [ ! $fails -eq 0 ]; then
#    printf "Warning: Failed to compile something.\n"
#    exit -1
# fi

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
               --enable-ffplay \
	       --disable-shared \
	       --enable-gpl \
	       --enable-nonfree \
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
