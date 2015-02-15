#!/usr/bin/env sh

mkdir --parents \
      $HOME/git/installs \
      $HOME/.local/bin

function android_sdk()
{
    url=http://dl.google.com/android/android-sdk_r24.0.2-linux.tgz
    if [ ! -d $HOME/git/installs/android_sdk ]; then
	wget $url --output-document=$HOME/git/installs/android_sdk.tgz --quiet
	(cd $HOME/git/installs &&
		tar xvf android_sdk.tgz &&
		mv android-sdk-linux android_sdk &&
		rm ./android_sdk.tgz)
    fi
}

function android_ndk()
{
    url=http://dl.google.com/android/ndk/android-ndk-r10d-linux-x86_64.bin
    if [ ! -d $HOME/git/installs/android_ndk ]; then
	wget $url --output-document=$HOME/git/installs/android_ndk.bin --quiet
	(cd $HOME/git/installs &&
		chmod a+x android_ndk.bin &&
		./android_ndk.bin &&
		mv android-ndk-linux android_ndk &&
		rm ./android_ndk.bin)
    fi
}

function install_repo()
{
    if [ ! -x $HOME/.local/bin/repo ]; then
	wget https://storage.googleapis.com/git-repo-downloads/repo \
	     --output-document=$HOME/.local/bin/repo --quiet
	chmod a+x $HOME/.local/bin/repo
    fi
}

pids=()
fails=0

android_sdk &
pids+=($!)
android_ndk &
pids+=($!)
install_repo &
pids+=($!)

# Wait for the SDK/NDK to be installed.
for pid in "${pids[@]}"; do
    wait $pid || let "fails+=1"
done
