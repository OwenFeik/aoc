def priority(item):
    if item.isupper():
        return ord(item) + 27 - ord('A')
    else:
        return ord(item) + 1 - ord('a')
 

with open('day3.txt', 'r') as f:
    data = filter(bool, f.read().split('\n'))

part1 = 0
part2 = 0

group = []
for rucksack in data:
    m = len(rucksack) // 2
    left, right = rucksack[:m], rucksack[m:]
    shared = [i for i in left if i in right][0]
    part1 += priority(shared)

    group.append(rucksack)
    if len(group) == 3:
        r1, r2, r3 = group
        group = []
        badge = [i for i in r1 if i in r2 and i in r3][0]
        part2 += priority(badge)

print('Part 1:', part1)
print('Patt 2:', part2)

