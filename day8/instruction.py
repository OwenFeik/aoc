base_program = []

with open('in.txt', 'r') as f:
    for line in f:
        instruction, argument = line.replace('\n', '').split()
        base_program.append((instruction, int(argument)))


def replace_kth(k):
    program = base_program[:]
    j = 0
    for i in range(len(base_program)):
        instruction, argument = base_program[i]

        if instruction == 'acc':        
            continue

        j += 1
        if not (j > k):
            continue

        elif instruction == 'jmp':
            program[i] = ('nop', argument)
        elif instruction == 'nop':
            program[i] = ('jmp', argument) 

        return program

visited = []
program = base_program[:]
accumulator = 0
e = len(base_program)
k = 0
i = 0

while True:
    # print(i)
    if i == e:
        print(accumulator)
        exit()
    elif i > e or i in visited:
        visited = []
        program = replace_kth(k)
        k += 1
        accumulator = 0
        i = 0
        continue

    visited.append(i)
    instruction, argument = program[i]

    if instruction == 'nop':
        i += 1
    elif instruction == 'acc':
        i += 1
        accumulator += argument
    elif instruction == 'jmp':
        i += argument
    else:
        raise ValueError(f'Unknown instruction: {instruction}')
