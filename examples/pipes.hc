struct XorWowState {
    i64* state;
};

i64 xorwow(struct XorWowState* state) {
    i64 s, t = state->state[3];
    t = xor(t, t / 4);
    t = xor(t, t * 2);
    state->state[3] = state->state[2] = state->state[1] = s = state->state[0];
    t = xor(t, s);
    t = xor(t, s * 16);
    state->state[0] = t;
    return t + (state->state[4] += 362437);
}

void seed_rng(struct XorWowState* rng) {
    rng->state = malloc(sizeof i64 * 5);
    rng->state[0] = -123356245;
    rng->state[1] = 158809235;
    rng->state[2] = 120035555;
    rng->state[3] = 154124;
    rng->state[4] = 11318611;
}

#define ROWS 48
#define COLS 160

#define BACKGROUNDCOLORS 0

#define WHITE 37+BACKGROUNDCOLORS
#define CYAN 36+BACKGROUNDCOLORS
#define MAGENTA 35+BACKGROUNDCOLORS
#define BLUE 34+BACKGROUNDCOLORS
#define YELLOW 33+BACKGROUNDCOLORS
#define GREEN 32+BACKGROUNDCOLORS
#define RED 31+BACKGROUNDCOLORS

#define U 0
#define R 1
#define D 2
#define L 3



/*
  0 -> UP/DOWN => UP/DOWN
  1 -> LEFT/RIGHT => LEFT/RIGHT
  2 -> UP/RIGHT => LEFT/DOWN
  3 -> UP/LEFT => RIGHT/DOWN
  4 -> DOWN/LEFT => LEFT/UP
  5 -> DOWN/RIGHT => RIGHT/UP
 */
#define nPipes 2
#define colorMode 0

#define LBRACE 91
#define ESCAPE 27
#define SEMICOLON 59
#define H 72
#define J 24
#define m 109

struct Pipe {
  i64 x, y, color, track;
  i64 direction, lastDirection;
  i8* charset;
  struct XorWowState* rng;
  i64** map;

  u0 movecursor(i64 x, i64 y) {
    putch(ESCAPE);
    putch(LBRACE);
    puti64(y);
    putch(SEMICOLON);
    puti64(x);
    putch(H);
  }

  i64 getColor() {
    return 31 + (this->rand() % 7);
  }

  i0 putcoloredc(i64 color, i8 str) {
    putch(ESCAPE);
    putch(LBRACE);
    puti64(color);
    putch(m);
    putch(str);
  }

  i0 init() {
    this->rng = new struct XorWowState;
    seed_rng(this->rng);
    this->charset = malloc(6);
    this->charset[0] = 124;
    this->charset[1] = 45;
    this->charset[2] = 92;
    this->charset[3] = 47;
    this->charset[4] = 47;
    this->charset[5] = 92; 

    this->map = malloc(4 * sizeof (i64*));

    this->map[0] = malloc(4 * sizeof(i64));
    this->map[0][0] = 0;
    this->map[0][1] = 3;
    this->map[0][2] = 0;
    this->map[0][3] = 2;

    this->map[1] = malloc(4 * sizeof(i64));
    this->map[1][0] = 4;
    this->map[1][1] = 1;
    this->map[1][2] = 2;
    this->map[1][3] = 1;

    this->map[2] = malloc(4 * sizeof(i64));
    this->map[2][0] = 0;
    this->map[2][1] = 5;
    this->map[2][2] = 0;
    this->map[2][3] = 4;

    this->map[3] = malloc(4 * sizeof(i64));
    this->map[3][0] = 5;
    this->map[3][1] = 1;
    this->map[3][2] = 3;
    this->map[3][3] = 1;
  }

  i8 randbool() {
    return this->rand() % 2;
  }

  i64 rand() {
    i64 p = xorwow(this->rng);
    if (p < 0) return p * -1;
    else return p;
  }

  i0 resetPipe() {
    if (this->randbool()) {
      if (this->randbool()) {
        this->y = 0;
        this->direction = D;
      } else {
        this->y = ROWS;
        this->direction = U;
      }
      this->x = this->rand() % (COLS-1);
    } else {
      if (this->randbool()) {
        this->x = 0;
        this->direction = R;
      } else {
        this->x = COLS;
        this->direction = L;
      }
      this->y = this->rand() % (ROWS-1);
    }
    this->track = 2 + (this->rand() % 8);
    this->color = this->getColor();
  }


  void turnPipe() {
    this->lastDirection = this->direction;
    if ((this->direction == U) + (this->direction == D)) {
      if (this->randbool()) this->direction = L;
      else if (this->randbool()) this->direction = R;
    } else {
      if (this->randbool()) this->direction = U;
      else if (this->randbool()) this->direction = D;
    }
    this->track = 2 + (this->rand() % 8);
  }
  
  /*
    0 -> UP/DOWN => UP/DOWN
    1 -> LEFT/RIGHT => LEFT/RIGHT
    2 -> UP/RIGHT => LEFT/DOWN
    3 -> UP/LEFT => RIGHT/DOWN
    4 -> DOWN/LEFT => LEFT/UP
    5 -> DOWN/RIGHT => RIGHT/UP
  */
  void printPipe() {
    i8 c = this->charset[this->map[this->lastDirection][this->direction]];
    this->movecursor(this->x, this->y);
    this->putcoloredc(this->color, c);
    this->lastDirection = this->direction;
  }
  
  void updatePipe() {
    this->track -= 1;
    if (this->track <= 0) {
      this->turnPipe();
    } else {
      if (this->direction == U) {
        this->y -= 1;
      } else if (this->direction == D) {
        this->y += 1;
      } else if (this->direction == L) {
        this->x -= 1;
      } else {
        this->x += 1;
      }
      if ((this->x <= 0) + (this->x >= COLS) +
          (this->y <= 0) + (this->y >= ROWS)) {
        this->resetPipe();
      }
    }
    this->printPipe(); 
  }
};

i64  main() {
  struct Pipe* pipe = new struct Pipe;
  pipe->init();
  pipe->resetPipe();
  i64 i = 0;
  while (i < 256) {
    putch(10);
    i += 1;
  }
  while (1) {
    pipe->updatePipe();
    flush();
    sleepms(50);
  }
}
