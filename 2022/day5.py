with open('day5.txt', 'r') as f:
    data = f.read().strip()

starting, moves = data.split('\n\n')

crates, _, labels = starting.strip().rpartition('\n')
n = int(labels.strip().split()[-1])
stacks = [[] for _ in range(n)]
lines = list(reversed(crates.split('\n')))
lines[-1] = lines[-1].rjust(len(lines[0]))
for i in range(n):
    for line in lines:
        if len(line) > 4 * i:
            crate = line[4 * i:4 * i + 3]
            if crate != '   ':
                stacks[i].append(crate[1])

stacks2 = [s[:] for s in stacks]

for move in moves.strip().split('\n'):
    _, n, _, a, _, b = move.split(' ')
    n = int(n); a = int(a); b = int(b)

    fr = stacks[a - 1]
    to = stacks[b - 1]
    for _ in range(n):
        stacks[b - 1].append(stacks[a - 1].pop())

    fr = stacks2[a - 1]
    to = stacks2[b - 1]
    to.extend(fr[-n:])
    del fr[-n:]

part1 = ''.join([s[-1] for s in stacks if s])
print('Part 1:', part1)
print('Part 2:', ''.join([s[-1] for s in stacks2 if s]))

