#!/usr/bin/env sh
# Perform post-installation configuration for Cygwin.

if ! grep -Fxq "db_home: /%H" /etc/nsswitch.conf; then
    printf "\ndb_home: /%%H\n" >> /etc/nsswitch.conf
fi

growl="$HOME/AppData/Local/Growl/2.0.0.0/user.config"
if [ -f  "$growl" ]; then
    sed -i -e 's|\(.*<value>\)Alt+X\(</value>\)|\1Alt+Shift+Y\2|g' $growl
fi
