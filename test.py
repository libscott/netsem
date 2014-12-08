import socket
import threading
import time


c = 0
m = 0
l = threading.Lock()


def measure():
    global c, m
    with l:
        c += 1
        m = max(c, m)
        print m
    time.sleep(0.1)
    with l:
        c -= 1


def work():
    sock = socket.socket()
    sock.connect(('127.0.0.1', 10000))
    sock.send("test.py 1000\n")
    d = sock.recv(3)
    assert d == 'go\n', repr(d)
    measure()
    sock.send("\n")
    sock.close()


for i in range(10000):
    threading.Thread(target=work).start()
