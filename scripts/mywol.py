#!/usr/bin/env python3

"""Send wake-on-lan packets."""

import socket
import sys

# Example use:
# mywol.py 70:71:BC:4A:D1:B4

mac = sys.argv[1].split(":")
mac = bytes(int(x, 16) for x in mac) * 16
magic = bytes(255 for _ in range(0, 6))
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
s.sendto(magic + mac*16, ('<broadcast>', 7))
