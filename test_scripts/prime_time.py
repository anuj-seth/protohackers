import socket
import json

client_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)


client_sock.connect(('127.0.1.1', 5432))
messages = [# {'method': 'isPrime', 'number':123 },
            # {'method': 'isPrime', 'number':123.0 },
            # {'method': 'isPrime', 'number':11.0 },
            # {'method': 'isPrime', 'number':123.2 },
            # {'method': 'isPrime', 'number':'123' },
            # {'method': 123, 'number':123 },
            # {'method': 'isPrime', 'number':1187 },
            {"method":"isPrime","number":134775908435340144855222017661958087420726727691935288,"bignumber":True},
            #"123 "
]

for message in messages:
    client_sock.sendall((json.dumps(message) + '\n').encode())

client_sock.sendall(b'n\n')
print("sent")

chunks = []
while True:
    data = client_sock.recv(2048)
    print(data.decode())
    if not data:
        break
    chunks.append(data)
    
print('Received', repr(b''.join(chunks)))

client_sock.shutdown(socket.SHUT_WR)

client_sock.close()
