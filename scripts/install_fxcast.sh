#!/usr/bin/env sh
# Install fx_cast and fx_cast_bridge.

# Manually install the deb/rpm package from:
# https://hensm.github.io/fx_cast/

cat <<EOF | sudo tee /etc/systemd/system/fx_cast.service > /dev/null
[Unit]
Description=fx_cast daemon

[Service]
User=fx_cast
ExecStart=/opt/fx_cast/fx_cast_bridge -d
Restart=always
RestartSec=15
KillSignal=SIGKILL

[Install]
WantedBy=multi-user.target
EOF
sudo useradd --system fx_cast
sudo systemctl enable fx_cast.service
