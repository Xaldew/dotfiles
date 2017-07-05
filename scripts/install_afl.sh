#/usr/bin/env sh
# Install the 'American Fuzzy Lop' (AKA AFL) fuzzer.

url=http://lcamtuf.coredump.cx/afl/releases/afl-latest.tgz
wget --output-document=$objects_dir/afl.tgz $url
mkdir --parents $objects_dir/afl
tar xzvf $objects_dir/afl.tgz --strip-components=1 --directory $objects_dir/afl -q
rm -f $objects_dir/afl.tgz
make -C $objects_dir/afl PREFIX=$local_prefix_dir install
