#!/usr/bin/env sh

# Install JDEE server component.
if [ ! -d $objects_dir/jdee ]; then
    git clone https://github.com/jdee-emacs/jdee-server.git $objects_dir/jdee
else
    git -C $objects_dir/jdee pull
fi

if command -v mvn; then
    cd $objects_dir/jdee
    mvn -Dmaven.test.skip=true package
    cp target/jdee-bundle-*.jar ${local_prefix_dir}/bin/
fi
