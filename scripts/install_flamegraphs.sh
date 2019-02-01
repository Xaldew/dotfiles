# Install the Flamegraph scripts.
if ! command -v flamegraph.pl; then
    git clone https://github.com/brendangregg/FlameGraph $objects_dir/flamegraph
    cp $objects_dir/flamegraph/*.{pl,awk} $local_prefix_dir/bin/
fi
