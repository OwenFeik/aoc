import re

class Bag():
    def __init__(self, colour):
        self.colour = colour
        # self.contained_by = []
        self.contained = []
        self.quantities = []

    def __repr__(self):
        return f'{self.colour} bag'

def parse(rules):
    graph = {}
    exp = re.compile(r'^(?P<o>[a-z ]+) contain (?P<c>[\da-z\, ]+)\.$')
    for line in rules:
        if not line:
            continue

        m = re.match(exp, re.sub(r'bags?', '', line))
        outer = m.group('o').strip()
        contents = [b.strip() for b in m.group('c').split(', ')]

        if not outer in graph:
            graph[outer] = Bag(outer)

        for b in contents:
            if b == 'no other':
                continue

            qty, colour = b.split(maxsplit=1)
            qty = int(qty)

            if not colour in graph:
                graph[colour] = Bag(colour)

            graph[outer].quantities.append(qty)
            # graph[colour].contained_by.append(graph[outer])
            graph[outer].contained.append(graph[colour])

    count = -1 # exclude root
    # visited = []
    to_visit = [graph['shiny gold']]

    while to_visit:
        visit_next = []
        for b in to_visit:
            # if not b in visited:
                # visited.append(b)
                # for c in b.contained_by:
            for q, c in zip(b.quantities, b.contained):
                for _ in range(q):                
                    visit_next.append(c)

            count += 1
        
        to_visit = visit_next
    
    # visited.remove(graph['shiny gold'])
    # print(len(visited))

    print(count)

with open('in.txt', 'r') as f:
    parse(f.read().split('\n'))
