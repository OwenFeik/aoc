joltages = []

with open('in.txt', 'r') as f:
    for l in f:
        joltages.append(int(l[:-1]))

joltages.append(0)
joltages.append(max(joltages) + 3)
joltages = sorted(joltages)


def part_one():
    deltas = {}
    for i in range(1, len(joltages)):
        delta = joltages[i] - joltages[i - 1]
        if delta not in deltas:
            deltas[delta] = 0
        deltas[delta] += 1

    print(deltas)

def part_two():
    n = len(joltages) - 1

    def dfs(i, memo={}):
        if i in memo:
            return memo[i]
            
        solns = 0

        if i == n:
            return 1

        for j in range(i + 1, len(joltages)):
            if joltages[j] - joltages[i] in [1, 2, 3]:
                print(j)
                solns += dfs(j)

        memo[i] = solns
        return solns

    print(dfs(0))

part_two()
