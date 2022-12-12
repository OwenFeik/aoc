from heapq import nlargest

with open('day1.txt', 'r') as f:
    data = f.read()

elves = list(map(
    lambda s: sum(map(int, s.split('\n'))),
    filter(bool, data.split('\n\n'))
))

print('Part 1: ', max(elves), 'Calories')
print('Part 2: ', sum(nlargest(3, elves)), 'Calories')

