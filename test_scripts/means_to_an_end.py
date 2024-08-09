#!/usr/bin/env python3
import socket
import struct
import binascii

#format = "!cii"
#binary_data = struct.pack(format, b'I', 12345, 101)
#print(len(binary_data))
#print(struct.calcsize(format))

#print(binascii.unhexlify("490000303900000065"))

client_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

client_sock.connect(('127.0.1.1', 5432))

messages = [
    "490000303900000065", #  I 12345 101
    #"490000303a00000066", #  I 12346 102
    #"490000303b00000064", #  I 12347 100
    #"490000a00000000005", #  I 40960 5

    "490F0D2744FFFFFF32"
]
#--> 00 00 00 65                  101
for message in messages:
    client_sock.sendall(binascii.unhexlify(message))
print("sent")

client_sock.sendall(binascii.unhexlify("510000300000004000")) #  Q 12288 16384
data = client_sock.recv(4)
#     print(data.decode())
#     if not data:
#         break
#     chunks.append(data)
    
print('Received ', binascii.hexlify(data))


client_sock.sendall(binascii.unhexlify("51000030000F0D2744"))
data = client_sock.recv(4)
#     print(data.decode())
#     if not data:
#         break
#     chunks.append(data)
    
print('Received ', binascii.hexlify(data))

client_sock.shutdown(socket.SHUT_WR)

client_sock.close()
