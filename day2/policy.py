import re

with open('in.txt', 'r') as f:
    data = [] # (min, max, char, pass)
    for l in f:
        m = re.match(r'^(?P<i>\d+)-(?P<a>\d+) ?(?P<c>\w): ?(?P<p>\w+)\n?', l)
        if m:
            data.append(
                (
                    int(m.group('i')),
                    int(m.group('a')),
                    m.group('c'),
                    m.group('p')
                )
            )

def part_one():
    valid = 0
    for i, a, c, p in data:
        if i <= p.count(c) <= a:
            valid += 1
    
    return valid

def part_two():
    valid = 0
    for a, b, c, p in data:
        n = int(p[a - 1] == c) + int(p[b - 1] == c)
        if n == 1:
            valid += 1

    return valid

print(part_two())
