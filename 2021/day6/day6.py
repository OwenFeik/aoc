import functools
import sys

with open(sys.argv[1], "r") as f:
    fishes = map(int, f.read().split(","))

@functools.lru_cache
def pop(time):
    if time - 7 > 0:
        return pop(time - 7) + pop(time - 9)
    return 1


def part2(fishes, time):
    return sum(map(lambda f: pop(time + (7 - f)), fishes))

print(part2(fishes, 256))
