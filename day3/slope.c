#include <stdio.h>
#include <stdlib.h>

#define LINE_WIDTH 31
#define TREE '#'

int part_one(void) {
    int dx = 3, dy = 1;
    int x = 0, t = 0;
    char line[LINE_WIDTH];

    while (1) {
        int i = 0;
        while (i++ < dy) {
            int read = getline(&line, 0, stdin);

            if (!read) {
                printf("Trees: %d\n", t);
                return 0;
            }
        }

        printf("%s\n", line);

        if (line[x % LINE_WIDTH] == TREE) {
            t += 1;
        }

        x += dx;
    }
 
}

int main(void) {
    part_one();    
}
