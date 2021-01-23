#include <stddef.h>

void update_history(int n, int turn, int *(history[][])) {
  int * a = history[n];
  if (a == NULL)
    history[n] = {turn, NULL};
  else
    history[n] = {turn, a[0]};
}

int main() {

    int turn = 0;
    int * last_spoken = NULL;
    //int puzzle_input[] = {8,13,1,0,18,9};
    int puzzle_input[] = {0, 3, 6};

    int end_turn = 2020;
    int history[end_turn][2];

    for (int i = 0; i < end_turn; i++)
      history[i][0] = NULL;

}
