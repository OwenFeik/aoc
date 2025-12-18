from functools import reduce
import z3

def solve_problem(problem: tuple[list[list[int]], list[int]]) -> int:
    (buttons, joltages) = problem
    variables = [z3.Int(f'b{i}') for i in range(0, len(buttons))]
    
    solver = z3.Optimize()
    for var in variables:
        solver.add(var >= 0)
    for (i, joltage) in enumerate(joltages):
        vars = [v for (v, b) in zip(variables, buttons) if i in b]
        terms = reduce(lambda b1, b2: b1 + b2, vars)
        solver.add(terms == joltage)
    solver.minimize(reduce(lambda a, b: a + b, variables))
    assert solver.check() == z3.sat

    model = solver.model()
    total = 0
    for var in variables:
        value = model[var]
        assert isinstance(value, z3.IntNumRef)
        total += value.py_value()
    return total

def parse_nums_in_brackets(text: str) -> list[int]:
    return list(map(int, text[1:-1].split(',')))

problems: list[tuple[list[list[int]], list[int]]] = []
with open('input.txt', 'r') as f:
    for line in f:
        parts = line.strip().split()
        buttons = list(map(parse_nums_in_brackets, parts[1:-1]))
        joltages = parse_nums_in_brackets(parts[-1])
        problems.append((buttons, joltages))

total = 0
for problem in problems:
    total += solve_problem(problem)
print("Part 2:", total)

