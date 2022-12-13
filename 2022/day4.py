with open('day4.txt', 'r') as f:
    data = f.read().strip().split('\n')

part1 = 0
part2 = 0

for pair in data:
    a, b = pair.split(',')
    a1, a2 = map(int, a.split('-'))
    b1, b2 = map(int, b.split('-'))
    if (a1 <= b1 and a2 >= b2) or (b1 <= a1 and b2 >= a2):
        part1 += 1
        part2 += 1
    elif (a1 <= b1 and a2 >= b1) or (a1 <= b2 and a2 >= b2):
        part2 += 1

print('Part 1:', part1, 'Ranges')
print('Part 2:', part2, 'Ranges')

