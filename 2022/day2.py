with open('day2.txt', 'r') as f:
    data = f.read()

moves = {'X': 1, 'Y': 2, 'Z': 3}
translate = {'A': 'X', 'B': 'Y', 'C': 'Z'}

part1 = 0
part2 = 0
for line in data.split('\n'):
    if not line:    
        continue

    a, b = line.split()
    
    part1 += moves[b]
    part1 += {
        ('A', 'X'): 3,
        ('B', 'X'): 0,
        ('C', 'X'): 6,
        ('A', 'Y'): 6,
        ('B', 'Y'): 3,
        ('C', 'Y'): 0,
        ('A', 'Z'): 0,
        ('B', 'Z'): 6,
        ('C', 'Z'): 3
    }[(a, b)]

    if b == 'X':
        if a == 'A':
            move = 'Z'
        elif a == 'B':
            move = 'X'
        elif a == 'C':
            move = 'Y'
        part2 += 0
    elif b == 'Y':
        move = translate[a]
        part2 += 3
    else:
        if a == 'A':
            move = 'Y'
        elif a == 'B':
            move = 'Z'
        elif a == 'C':
            move = 'X'
        part2 += 6
    part2 += moves[move]
    
print('Part 1:', part1)
print('Part 2:', part2)
