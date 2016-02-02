#!/usr/bin/env sh

android_sdk()
{
    url=http://dl.google.com/android/android-sdk_r24.4.1-linux.tgz
    if [ ! -d $objects_dir/android_sdk ]; then
	wget $url --output-document=$objects_dir/android_sdk.tgz --quiet
	(cd $objects_dir &&
		tar xvf android_sdk.tgz &&
		mv android-sdk-linux android_sdk &&
		rm ./android_sdk.tgz)
    fi
}

android_ndk()
{
    url=http://dl.google.com/android/ndk/android-ndk-r10e-linux-x86_64.bin
    if [ ! -d $objects_dir/android_ndk ]; then
	wget $url --output-document=$objects_dir/android_ndk.bin --quiet
	(cd $objects_dir &&
		chmod a+x android_ndk.bin &&
		./android_ndk.bin &&
		mv android-ndk-r10e android_ndk &&
		rm ./android_ndk.bin)
    fi
}

install_repo()
{
    if [ ! -x $local_prefix_dir/bin/repo ]; then
	wget https://storage.googleapis.com/git-repo-downloads/repo \
	     --output-document=$local_prefix_dir/bin/repo --quiet
	chmod u+x $local_prefix_dir/bin/repo
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
