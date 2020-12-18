joltages = []

with open('in.txt', 'r') as f:
    for l in f:
        joltages.append(int(l[:-1]))

joltages.append(0)
joltages.append(max(joltages) + 3)
joltages = sorted(joltages)

print(joltages)

deltas = {}
for i in range(1, len(joltages)):
    delta = joltages[i] - joltages[i - 1]
    if delta not in deltas:
        deltas[delta] = 0
    deltas[delta] += 1

print(deltas)
