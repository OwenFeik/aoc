#include <stdio.h>
#include <stdlib.h>

#define SIZE_Y 92
#define SIZE_X 91
#define PART_TWO

int main(int argc, char** argv) {
    char** lines = (char**) malloc(sizeof(char*) * SIZE_Y);
    char** next = (char**) malloc(sizeof(char*) * SIZE_Y);

    for (int i = 0; i < SIZE_Y; i++) {
        lines[i] = (char*) malloc(sizeof(char) * SIZE_X + 1);
        next[i] = (char*) malloc(sizeof(char) * SIZE_X + 1);
        scanf("%s\n", lines[i]);
    }

    int changed = 1;
    while (changed) {
        changed = 0;

        for (int y = 0; y < SIZE_Y; y++) {
            for (int x = 0; x < SIZE_X; x++) {
                printf("%c", lines[y][x]);

                #ifdef PART_ONE
                int adj = 0;

                int neg_x = x - 1 >= 0;
                int neg_y = y - 1 >= 0;
                int pos_x = x + 1 < SIZE_X;
                int pos_y = y + 1 < SIZE_Y;

                if (neg_x) {
                    if (neg_y && lines[y - 1][x - 1] == '#') {
                        adj++;
                    }
                    if (lines[y][x - 1] == '#') {
                        adj++;
                    }
                    if (pos_y && lines[y + 1][x - 1] == '#') {
                        adj++;
                    }
                }
                if (pos_x) {
                    if (neg_y && lines[y - 1][x + 1] == '#') {
                        adj++;
                    }
                    if (lines[y][x + 1] == '#') {
                        adj++;
                    }
                    if (pos_y && lines[y + 1][x + 1] == '#') {
                        adj++;
                    }
                }
                if (neg_y && lines[y - 1][x] == '#') {
                    adj++;
                }
                if (pos_y && lines[y + 1][x] == '#') {
                    adj++;
                }

                if (lines[y][x] == 'L' && adj == 0) {
                    next[y][x] = '#';
                    changed = 1;
                }
                else if (lines[y][x] == '#' && adj >= 4) {
                    next[y][x] = 'L';
                    changed = 1;
                }
                else {
                    next[y][x] = lines[y][x];
                }
                #endif

                #ifdef PART_TWO
                int visible = 0;

                int rn[8] = {1, 1, 1, 1, 1, 1, 1, 1};
                int dx[8] = {-1, 0, 1, -1, 1, -1, 1, 0};
                int dy[8] = {-1, -1, -1, 0, 0, 1, 1, 1};

                int running = 1, d = 1;
                while (running) {
                    running = 0;
                    for (int i = 0; i < 8; i++) {
                        if (!rn[i]) {
                            continue;
                        }
                        
                        int _x = x + d * dx[i];
                        int _y = y + d * dy[i];
                        
                        if (0 <= _x && _x < SIZE_X && 0 <= _y && _y < SIZE_Y) {
                            int c = lines[_y][_x]; 
                            if (c == '#') {
                                visible++;
                                rn[i] = 0;
                            }
                            else if (c == 'L') {
                                rn[i] = 0;
                            }
                        }
                        else {
                            rn[i] = 0;
                        }

                        running |= rn[i];
                    }
                    d++;
                }

                if (lines[y][x] == 'L' && visible == 0) {
                    next[y][x] = '#';
                    changed = 1;
                }
                else if (lines[y][x] == '#' && visible >= 5) {
                    next[y][x] = 'L';
                    changed = 1;
                }
                else {
                    next[y][x] = lines[y][x];
                }
                #endif
            }
            printf("\n");
        }

        printf("---\n");

        char** temp = lines;
        lines = next;
        next = temp;
    }

    int occupied = 0;
    for (int i = 0; i < SIZE_Y; i++) {
        for (int j = 0; j < SIZE_X; j++) {
            if (lines[i][j] == '#') {
                occupied++;
            }
        }
    }

    printf("%d\n", occupied);
}
