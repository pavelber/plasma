letter2ConfigHe = {
    "Y": "1s", "R": "2p", "R'": "3p",
    "C": "2s2p", "E": "2s2s", "F": "2p2p", "P": "1s2p", "S": "1s2s", "S'": "ls3s", "P'": "1s3p",
    "A'": "2p3d", "B'": "2p3s", "C'": "2s3p", "F'": "2p3p", "G'": "2s3d", "E'": "2s3s", "D'": "ls3d"
}

with open("c:\\work2\\plasma\\data\\MZ.csv", "rb") as f:
    f.readline()
    for l in f:
        parts = l.split(',')
        letterFrom = parts[9]
        numFrom = parts[10]
        letterTo = parts[11]
        numTo = parts[12]
