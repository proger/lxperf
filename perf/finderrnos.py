import struct
import sys
import errno

def tryerrno(part):
    if part.startswith('0xfff'):
        n = int(part[2:], 16)
        signed = struct.unpack('l', struct.pack('L', n))[0]
        return str(errno.errorcode.get(-signed, str(signed)))
    else:
        return part

if __name__ == '__main__':
    for line in sys.stdin:
        parts = line.split()
        print '\t'.join(tryerrno(part) for part in parts)
