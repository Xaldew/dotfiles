#!/usr/bin/env sh

mkdir --parents \
      $HOME/git/installs

function android_sdk()
{
    wget http://dl.google.com/android/android-sdk_r24.0.2-linux.tgz \
	 --output-document=$HOME/git/installs/android_sdk.tgz --quiet
    (cd $HOME/git/installs &&
	    tar xvf android_sdk.tgz &&
	    mv android-sdk-linux android_sdk)
}

function android_ndk()
{
    wget http://dl.google.com/android/ndk/android-ndk-r10d-linux-x86_64.bin \
	 --output-document=$HOME/git/installs/android_ndk.tgz --quiet
    (cd $HOME/git/installs &&
	    chmod a+x android_ndk.tgz &&
	    ./android_ndk.tgz &&
	    mv android-ndk-linux android_ndk)
}

pids=()
fails=0

android_sdk &
pids+=($!)
android_ndk &
pids+=($!)

# Wait for the SDK/NDK to be installed.
for pid in "${args[@]}"; do
    wait $pid || let "FAIL+=1"
done
