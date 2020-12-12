#include <stdio.h>
#include <stdlib.h>

#define LINE_WIDTH 31
#define TREE '#'

int count_trees(int dx, int dy) {
    int x = 0, t = 0;
    char line[LINE_WIDTH + 1];
    int first_loop = 1;

    while (1) {
        int i = 0;
        while (i++ < dy) {
            int j = 0;
            int c = ' ';
            while ((c = fgetc(stdin)) != '\n' && c != EOF) {
                line[j++] = c;
            }
            line[j] = '\0';

            if (i < dy) {
                printf("%s\n", line);
            }

            if (line[0] == '\0') {
                printf("Trees: %d\n", t);
                return 0;
            }

            if (first_loop) {
                first_loop = 0;
                break;
            }
        }

        if (line[x % LINE_WIDTH] == TREE) {
            t += 1;
            line[x % LINE_WIDTH] = 'X';
        }
        else {
            line[x % LINE_WIDTH] = 'O';
        }

        printf("%s\n", line);

        x += dx;
    }
 
}

int part_one() {
    count_trees(3, 1);
}

int part_two() {
    // count_trees(1, 1);
    // count_trees(3, 1);
    // count_trees(5, 1);
    // count_trees(7, 1);
    count_trees(1, 2);
}

int main(void) {
    part_two();    
}
