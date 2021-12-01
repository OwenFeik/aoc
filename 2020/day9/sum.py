values = []
with open('in.txt', 'r') as f:
    for l in f:
        values.append(int(l[:-1]))

sums = []
for v in values:
    for o in values:
        if v != o:
            sums.append(v + o)
        else:
            sums.append(0)


i = 25
while True:
    # print(sums, len(sums), 45 in sums)
    if values[i] not in sums:
        val = values[i]
        break

    i += 1

    sums = sums[25:]

del sums

running = True
i = 0
while runing:
    total = 0
    j = 0
    while True:
        total += values[i + j]
        j += 1
        
        if total == val:
            soln = values[i : i + j]
            running = False
                
        elif total > val:
            total = 0
            break
    i += 1

print(soln)
print(min(soln) + max(soln))
