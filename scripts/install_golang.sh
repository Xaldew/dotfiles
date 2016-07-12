#!/usr/bin/env sh
# Install the Go language toolchain.

rm -rf $objects_dir/golang
url=https://storage.googleapis.com/golang/go1.6.2.linux-amd64.tar.gz
wget --quiet --output-document=$objects_dir/golang.tar.gz $url
tar -C $objects_dir/ -xzf $objects_dir/golang.tar.gz
mv $objects_dir/go $objects_dir/golang
rm $objects_dir/golang.tar.gz
