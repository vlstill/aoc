import sys
from itertools import chain

def pt(spt):
    x, y = spt.split(",")
    return int(x), int(y)

lines = [tuple(sorted([pt(a), pt(b)])) for (a, b) in (raw.strip().split(" ->" ) for raw in sys.stdin.readlines())]

max_x = max(chain.from_iterable((x1, x2) for (x1, _), (x2, _) in lines))
max_y = max(chain.from_iterable((y1, y2) for (_, y1), (_, y2) in lines))

board = [[0 for _ in range(max_x + 1)] for _ in range(max_y + 1)]

def isV(line): return line[0][0] == line[1][0]
def isH(line): return line[0][1] == line[1][1]

def isrange(a, b): return range(min(a, b), max(a, b) + 1)

for line in lines:
    (x1, y1), (x2, y2) = line
    if isV(line):
        for y in isrange(y1, y2):
            board[y][x1] += 1
    if isH(line):
        for x in isrange(x1, x2):
            board[y1][x] += 1

# print("\n".join("".join(str(v) for v in line) for line in board))
print(sum(x >= 2 for x in chain.from_iterable(board)))

def sig(x):
    if (x > 0):
        return 1
    if (x < 0):
        return -1
    return 0

for line in lines:
    (x1, y1), (x2, y2) = line
    sx = sig(x2 - x1)
    sy = sig(y2 - y1)
    mx = min(x1, x2, key=lambda x: x * sx)
    my = min(y1, y2, key=lambda y: y * sy)
    if abs(x1 - x2) == abs(y1 - y2):
        # print(line)
        for i in range(abs(x1 - x2) + 1):
            board[my + i * sy][mx + i * sx] += 1
# print("\n".join("".join(str(v) for v in line) for line in board))
print(sum(x >= 2 for x in chain.from_iterable(board)))  
