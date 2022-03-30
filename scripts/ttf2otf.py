#!/usr/bin/env python3
import sys
import fontforge
font = fontforge.open(sys.argv[1])
font.generate(sys.argv[2])
