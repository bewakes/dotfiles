import base64
import sys
import getpass


def xor_block(key, text):
    """Returns bytes"""
    if type(text) == str:
        text = text.encode()
    if type(key) == str:
        key = key.encode()
    zipped = zip(key, text)
    output = b''
    for k, v in zipped:
        output += chr(k ^ v).encode()
    return output


def encrypt(key, text):
    lenkey = len(key)
    textsp = [
        text[i*lenkey:(i+1)*lenkey]
        for i, x in enumerate(range(0, len(text), lenkey))
    ]
    joined = b''.join(list(map(lambda x: xor_block(key, x), textsp)))
    return base64.b64encode(joined).decode()


def decrypt(key, b64txt):
    text = base64.b64decode(b64txt)
    lenkey = len(key)
    textsp = [
        text[i*lenkey:(i+1)*lenkey]
        for i, x in enumerate(range(0, len(text), lenkey))
    ]
    joined = b''.join(list(map(lambda x: xor_block(key, x), textsp)))
    return joined.decode()


if __name__ == '__main__':
    args = sys.argv
    if not args or len(args) < 3:
        print("usage: python encdec.py [e, d] <filename>")
        exit(1)
    password = getpass.getpass("Enter key to unlock: ")
    action = encrypt if args[1] == 'e' else decrypt
    filename = args[2]

    f = open(filename, 'r')
    print(action(password, f.read()))
