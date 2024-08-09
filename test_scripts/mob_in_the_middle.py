#!/usr/bin/env python3
import socket
import time
from multiprocessing import Process
ip_address = '127.0.1.1' #'20.26.230.111'

def got (prefix, message):
    print("<-- " + prefix + ": " + message)

def sent (prefix, message):
    print("--> " + prefix + ": " + message)

def client_bob (prefix):
    client_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    client_sock.connect((ip_address, 5432))
    welcome_message = client_sock.recv(2048)
    got(prefix, welcome_message.decode())

    username = b"bob\n"
    sent(prefix, username.decode())
    client_sock.sendall(username)

    room_contains = client_sock.recv(2048)
    got(prefix, room_contains.decode())
    if "* The room contains" not in room_contains.decode():
        print("Did not get * The room contains")
        return
    time.sleep(5)
    client_sock.sendall(b"hello world\n")
    client_sock.sendall(b"hello world 2\n")
    client_sock.sendall(b"hello world 3\n")
    client_sock.shutdown(socket.SHUT_WR)
    client_sock.close()

def after_bob (prefix):
    client_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    client_sock.connect((ip_address, 5432))
    welcome_message = client_sock.recv(2048)
    got(prefix, welcome_message.decode())

    username = b"hello1234567890world\n"
    sent(prefix, username.decode())
    client_sock.sendall(username)

    room_contains = client_sock.recv(2048)
    got(prefix, room_contains.decode())
    if "* The room contains" not in room_contains.decode():
        print("Did not get * The room contains")
        return

    message = client_sock.recv(2048)
    got(prefix, message.decode())

    client_sock.sendall(b"hello world 4\n")
    client_sock.sendall(b"hello world 5\n")
    client_sock.sendall(b"hello world 6\n")
    client_sock.shutdown(socket.SHUT_WR)
    client_sock.close()

def client_only_welcome (prefix):
    client_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    client_sock.connect((ip_address, 5432))
    welcome_message = client_sock.recv(2048)
    got(prefix, welcome_message.decode())
    
    while True:
        data = client_sock.recv(2048)
        got(prefix, data.decode())

    client_sock.shutdown(socket.SHUT_WR)
    client_sock.close()

if __name__ == "__main__":
    only_welcome = Process(target=client_only_welcome, args=("ONLY WELCOME",))
    only_welcome.start()
    
    bob = Process(target=client_bob, args=("BOB",))
    bob.start()

    time.sleep(2)
    long_username = Process(target=after_bob, args=("AFTER BOB",))
    long_username.start()

    only_welcome.join()
    bob.join()
    long_username.join()

