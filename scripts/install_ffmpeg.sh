#!/usr/bin/env sh
# A script to download and install the latest ffmpeg and all related libraries.

sudo apt-get install --quiet=2 \
     nasm

mkdir --parents \
      $HOME/git/installs/ffmpeg \
      $HOME/git/installs/ffmpeg/ffmpeg_build

install_yasm()
{
    # Compile and install yasm.
    cd $HOME/git/installs/ffmpeg
    if [ ! -d yasm ]; then
	git clone git://github.com/yasm/yasm.git
    else
	git -C yasm pull
    fi
    cd $HOME/git/installs/ffmpeg/yasm
    ./autogen.sh
    ./configure \
	--prefix=$HOME/git/installs/ffmpeg_build \
	--bindir=$HOME/.local/bin
    make
    make install
    make distclean
}

install_libx264()
{
    # Compile and install libx264.
    cd $HOME/git/installs/ffmpeg
    if [ ! -d libx264 ]; then
	git clone git://git.videolan.org/x264.git
    else
	git -C libx264 pull
    fi
    cd $HOME/git/installs/ffmpeg/x264
    ./configure \
	--prefix=$HOME/git/installs/ffmpeg/ffmpeg_build \
	--enable-static
    make
    make install
    make distclean
}

install_libx265()
{
    # Compile and install libx265 (HEVC)
    cd $HOME/git/installs/ffmpeg
    if [ ! -d libx265 ]; then
	hg clone https://bitbucket.org/multicoreware/x265
    else
	git -C libx265 pull
    fi
    cd $HOME/git/installs/ffmpeg/x265/build/linux
    cmake \
	-DCMAKE_INSTALL_PREFIX=$HOME/git/installs/ffmpeg/ffmpeg_build \
	-DENABLE_SHARED=off \
	../../source
    make
    make install
    make clean
}

install_libvpx()
{
    # Download, compile and install libvpx.
    cd $HOME/git/installs/ffmpeg
    if [ ! -d libvpx ]; then
	git clone git://github.com/webmproject/libvpx.git
    else
	git -C libvpx pull
    fi
    cd libvpx
    ./configure \
	--prefix=$HOME/git/installs/ffmpeg/ffmpeg_build \
	--disable-examples
    make
    make install
    make clean
}

install_libopus()
{
    # Install Opus audio codecs.
    cd $HOME/git/installs/ffmpeg
    if [ ! -d opus ]; then
	git clone git://git.opus-codec.org/opus.git
    else
	git -C opus pull
    fi
    cd opus
    ./autogen.sh
    ./configure \
	--prefix=$HOME/git/installs/ffmpeg/ffmpeg_build \
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
    cd $HOME/git/installs/ffmpeg
    if [ ! -d lame ]; then
	mkdir --parents lame
	wget $url --output-document=lame.tar.gz --quiet
	tar xzvf lame.tar.gz -C lame --strip-components=1
	rm lame.tar.gz
    fi
    cd lame
    ./configure \
	--prefix=$HOME/git/installs/ffmpeg/ffmpeg_build \
	--enable-nasm \
	--disable-shared
    make
    make install
    make distclean
}

install_fdk_aac()
{
    # Install fdk-AAC audio codec.
    cd $HOME/git/installs/ffmpeg
    if [ ! -d fdk-aac ]; then
	git clone git://github.com/mstorsjo/fdk-aac.git
    else
	git -C fdk-aac pull
    fi
    cd fdk-aac
    autoreconf -fiv
    ./configure \
	--prefix=$HOME/git/installs/ffmpeg/ffmpeg_build \
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


# Finally, compile and install ffmpeg.
cd $HOME/git/installs/ffmpeg
git clone git://github.com/FFmpeg/FFmpeg.git ffmpeg_sources
cd $HOME/git/installs/ffmpeg/ffmpeg_sources
PKG_CONFIG_PATH=$HOME/git/installs/ffmpeg/ffmpeg_build/lib/pkgconfig \
	       ./configure \
	       --prefix=$HOME/git/installs/ffmpeg/ffmpeg_build \
	       --bindir=$HOME/.local/bin \
	       --mandir=$HOME/.local/share/man \
	       --pkg-config-flags="--static" \
	       --extra-cflags=-I$HOME/git/installs/ffmpeg/ffmpeg_build/include \
	       --extra-ldflags=-L$HOME/git/installs/ffmpeg/ffmpeg_build/lib \
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
