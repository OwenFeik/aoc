with open('in.txt', 'r') as f:
    data = [int(l) for l in f.read().split('\n') if l]

def part_one():
    for v in data:
        for o in data:
            if v + o == 2020:
                return v * o

def part_two():
    # n ** 3? psh, I eat n ** 3 for breakfast.

    for i in data:
        for j in data:
            for k in data:
                if i + j + k == 2020:
                    return i * j * k                    

print(part_two())
