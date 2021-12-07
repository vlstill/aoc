import sys

data = list(map(int, sys.stdin.readline().split(',')))

print(min((sum(abs(p - c) for c in data), p) for p in range(min(data), max(data) + 1)))
print(min((sum(sum(range(abs(p - c) + 1)) for c in data), p) for p in range(min(data), max(data) + 1)))
