#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

#define clearscreen() printf("\033[H\033[J")
#define movecursor(x, y) printf("\033[%d;%dH", (y), (x))
#define printcoloredstr(ch, color) printf("\033[%dm%s", color, ch)
#define randbool() (rand()&1)
int ROWS = 24;
int COLS = 80;

#define BACKGROUNDCOLORS 0

#define WHITE 37+BACKGROUNDCOLORS
#define CYAN 36+BACKGROUNDCOLORS
#define MAGENTA 35+BACKGROUNDCOLORS
#define BLUE 34+BACKGROUNDCOLORS
#define YELLOW 33+BACKGROUNDCOLORS
#define GREEN 32+BACKGROUNDCOLORS
#define RED 31+BACKGROUNDCOLORS



/*
  0 -> UP/DOWN => UP/DOWN
  1 -> LEFT/RIGHT => LEFT/RIGHT
  2 -> UP/RIGHT => LEFT/DOWN
  3 -> UP/LEFT => RIGHT/DOWN
  4 -> DOWN/LEFT => LEFT/UP
  5 -> DOWN/RIGHT => RIGHT/UP
 */
char* charSets[] = {"┃", "━", "┓", "┏", "┛", "┗"};
int charset = 0;
int nPipes = 2;
int colorMode = 0;



int getColor() {
  static const int map[][6] = {
    {WHITE, RED, GREEN, YELLOW, CYAN, WHITE}
  };
  if (colorMode == 0) return map[0][rand() % 6];
    
  else
  return 31 + (rand() % 7);
}

typedef struct Pipe {
  int x, y, color, track;
  enum { U = 0, R = 1, D = 2, L = 3 } direction, lastDirection;
} Pipe;

void resetPipe(Pipe* p) {
  if (randbool()) {
    if (randbool()) {
      p->y = 0;
      p->direction = D;
    } else {
      p->y = ROWS;
      p->direction = U;
    }
    p->x = rand() % (COLS-1);
  } else {
    if (randbool()) {
      p->x = 0;
      p->direction = R;
    } else {
      p->x = COLS;
      p->direction = L;
    }
    p->y = rand() % (ROWS-1);
  }
  p->track = 2 + (rand() % 8);
  p->color = getColor();
}

void turnPipe(Pipe* p) {
  p->lastDirection = p->direction;
  switch(p->direction) {
  case U:
  case D:
    if (randbool()) p->direction = L;
    else if (randbool()) p->direction = R;
    break;
  case L:
  case R:
    if (randbool()) p->direction = U;
    else if (randbool()) p->direction = D;
  }
  p->track = 2 + (rand() % 8);
}

/*
  0 -> UP/DOWN => UP/DOWN
  1 -> LEFT/RIGHT => LEFT/RIGHT
  2 -> UP/RIGHT => LEFT/DOWN
  3 -> UP/LEFT => RIGHT/DOWN
  4 -> DOWN/LEFT => LEFT/UP
  5 -> DOWN/RIGHT => RIGHT/UP
*/
void printPipe(Pipe* p) {
  // U R D L
  // 0 1 2 3
  static const int map[][4] = {{0, 3, 0, 2},
                               {4, 1, 2, 1},
                              {0, 5, 0, 4},
                              {5, 1, 3, 1}};
  char* c = charSets[map[p->lastDirection][p->direction]];
  movecursor(p->x, p->y);
  printcoloredstr(c, p->color);
  p->lastDirection = p->direction;
}

void updatePipe(Pipe* p) {
  p->track--;
  if (p->track <= 0) {
    turnPipe(p);
  } else {
    switch(p->direction) {
    case U:
      p->y -= 1;
      break;
    case D:
      p->y += 1;
      break;
    case L:
      p->x -= 1;
      break;
    case R:
      p->x += 1;
    }
    if (p->x <= 0 | p->x >= COLS |
        p->y <= 0 | p->y >= ROWS) {
      resetPipe(p);
    }
  }
  printPipe(p); 
}

void handleArgs(int argc, char** args) {
  for (int i = 0 ; i < argc ; i++) {
    if (strcmp("-c", args[i]) == 0 | strcmp("-C", args[i]) == 0) {
      if (i + 1 < argc) {
        charset = atoi(args[i + 1]);
      } else {
        printf("Usage: -c [0-4]");
        exit(0);
      }
    } else if (strcmp("-n", args[i]) == 0 | strcmp("-N", args[i]) == 0) {
      if (i + 1 < argc) {
        nPipes = atoi(args[i + 1]);
        if (nPipes < 1) nPipes = 1;
      } else {
        printf("Usage: -n [number of pipes]");
        exit(0);
      }
    } else if (strcmp("-h", args[i]) == 0 | strcmp("-H", args[i]) == 0) {
      if (i + 1 < argc) {
        ROWS = atoi(args[i + 1]);
if (ROWS < 10 || ROWS > 200) ROWS = 20;
      } else {
        printf("Usage: -h [number of rows]");
        exit(0);
      }
    } else if (strcmp("-W", args[i]) == 0 | strcmp("-w", args[i]) == 0) {
      if (i + 1 < argc) {
        COLS = atoi(args[i + 1]);
        if (COLS < 10 || COLS > 260) COLS = 10;
      } else {
        printf("Usage: -w [number of heights]");
        exit(0);
      }
    }
  }
}

int main(int argc, char** args) {
  handleArgs(argc, args);
  Pipe* pipes = malloc(sizeof(Pipe) * nPipes);
  for (int i = 0 ; i < nPipes ; i++) resetPipe(&pipes[i]);

  void handle(int signum) {
    clearscreen();
    printcoloredstr("\n", 30);
    printf("Goodbye :)\n");
    free(pipes);
    exit(0);
  }

  pid_t pid = fork();

  if (pid) {
    char _unused;
    scanf("%c", &_unused);
    free(pipes);
    kill(pid, SIGKILL);
    handle(0);
  } else {
    srand(time(NULL));

    signal(SIGINT, handle);
    struct timespec t1, t2;
    t1.tv_sec = 0;
    t1.tv_nsec = 1000000000 / 20; // 50 Milliseconds
    clearscreen();
    while (1) {
      for (int i = 0 ; i < nPipes ; i++) {
          updatePipe(&pipes[i]);
      }
      fflush(stdout);
      nanosleep(&t1, &t2);
      //movecursor(0, 0);
      //printf("x: %d ; y: %d ; track: %d", pipes[0].x, pipes[0].y, pipes[0].track);
    }
  }
}
