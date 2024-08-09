#!/usr/bin/env python3
import socket
import struct
import binascii
import time
import multiprocessing

def got (prefix, message):
    print("<-- " + prefix + ": " + message)

def sent (prefix, message):
    print("--> " + prefix + ": " + message)

def open_and_close ():
    client_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_sock.connect(('127.0.1.1', 5432))
    client_sock.close()

def send_unknown_message ():
    client_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_sock.connect(('127.0.1.1', 5432))
    message = "100000000a"
    print("sending unknown message")
    client_sock.sendall(binascii.unhexlify(message))
    print("sent")
    data = client_sock.recv(124)
    print('Received ', binascii.hexlify(data))
    client_sock.close()

def send_heartbeat_without_identifying ():
    client_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_sock.connect(('127.0.1.1', 5432))
    message = "400000000a" #  want heartbeat
    print("sending")
    client_sock.sendall(binascii.unhexlify(message))
    print("sent")
    data = client_sock.recv(124)
    print('Received ', binascii.hexlify(data))
    client_sock.close()

def negative_camera_wants_heartbeat ():
    client_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_sock.connect(('127.0.1.1', 5432))
    i_am_camera = "80"
    i_am_camera += "0042"  #               road: 66, \
    i_am_camera += "0064" #               mile: 100, \
    i_am_camera += "003c" #               limit: 60, \

    print("sending")
    client_sock.sendall(binascii.unhexlify(i_am_camera))
    print("sent")

    want_heartbeat = "40"
    want_heartbeat += "00000014"
    print("sending")
    client_sock.sendall(binascii.unhexlify(want_heartbeat))
    print(time.time(), "sent")
    print (time.time(), "waiting .....")
    time.sleep(10)
    client_sock.close()
    print(time.time(), "closed negative case")

def negative_camera_wants_heartbeat_unclean_close ():
    client_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_sock.connect(('127.0.1.1', 5432))
    i_am_camera = "80"
    i_am_camera += "0042"  #               road: 66, \
    i_am_camera += "0064" #               mile: 100, \
    i_am_camera += "003c" #               limit: 60, \

    print("sending")
    client_sock.sendall(binascii.unhexlify(i_am_camera))
    print("sent")

    want_heartbeat = "40"
    want_heartbeat += "00000014"
    print("sending")
    client_sock.sendall(binascii.unhexlify(want_heartbeat))
    print(time.time(), "sent")
    for i in range(3):
        data = client_sock.recv(124)
        print(time.time(), ' Received ', binascii.hexlify(data))

    client_sock.close()
    print(time.time(), "closed unclean")

def camera_wants_10_heartbeats ():
    client_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_sock.connect(('127.0.1.1', 5432))
    i_am_camera = "80"
    i_am_camera += "0042"  #               road: 66, \
    i_am_camera += "0064" #               mile: 100, \
    i_am_camera += "003c" #               limit: 60, \

    print("sending")
    client_sock.sendall(binascii.unhexlify(i_am_camera))
    print("sent")

    want_heartbeat = "40"
    want_heartbeat += "00000014"
    print("sending")
    client_sock.sendall(binascii.unhexlify(want_heartbeat))
    print("sent")
    for i in range(10):
        data = client_sock.recv(124)
        print(time.time(), ' Received ', binascii.hexlify(data))

    client_sock.close()

if __name__ == "__main__":
    processes = [multiprocessing.Process(target=f)
                 for f in [#open_and_close,
                           send_unknown_message,
                           #send_heartbeat_without_identifying
                         #negative_camera_wants_heartbeat,
                         #negative_camera_wants_heartbeat_unclean_close,
                         #camera_wants_10_heartbeats
                 ]]
    for p in processes:
        p.start()
    for p in processes:
        p.join()

