#!/usr/bin/env python3
import struct

format = "!cii"
binary_data = struct.pack(format, b'I', -4, 5)
print(binary_data)
print(struct.calcsize(format))
