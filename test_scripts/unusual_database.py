#!/usr/bin/env python3

import socket
 
UDP_IP = "127.0.1.1"
UDP_PORT = 5432

print("UDP target IP: %s" % UDP_IP)
print("UDP target port: %s" % UDP_PORT)
sock = socket.socket(socket.AF_INET, # Internet
                     socket.SOCK_DGRAM) # UDP
MESSAGE = b"version"
print("message: %s" % MESSAGE)
sock.sendto(MESSAGE, (UDP_IP, UDP_PORT))
data, addr = sock.recvfrom(1024) # buffer size is 1024 bytes
print("received message: %s" % data)


MESSAGE = b"key=value"
print("message: %s" % MESSAGE)
sock.sendto(MESSAGE, (UDP_IP, UDP_PORT))

MESSAGE = b"key=value2"
print("message: %s" % MESSAGE)
sock.sendto(MESSAGE, (UDP_IP, UDP_PORT))

MESSAGE = b"key"
print("message: %s" % MESSAGE)
sock.sendto(MESSAGE, (UDP_IP, UDP_PORT))
data, addr = sock.recvfrom(1024) # buffer size is 1024 bytes
print("received message: %s" % data)


MESSAGE = b"=empty key"
print("message: %s" % MESSAGE)
sock.sendto(MESSAGE, (UDP_IP, UDP_PORT))
MESSAGE = b""
print("message: %s" % MESSAGE)
sock.sendto(MESSAGE, (UDP_IP, UDP_PORT))
data, addr = sock.recvfrom(1024) # buffer size is 1024 bytes
print("received message: %s" % data)

MESSAGE = b"empty value="
print("message: %s" % MESSAGE)
sock.sendto(MESSAGE, (UDP_IP, UDP_PORT))
MESSAGE = b"empty value"
print("message: %s" % MESSAGE)
sock.sendto(MESSAGE, (UDP_IP, UDP_PORT))
data, addr = sock.recvfrom(1024) # buffer size is 1024 bytes
print("received message: %s" % data)
