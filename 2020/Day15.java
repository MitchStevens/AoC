class Day15 {
    static int turn = 0;
    static int last_spoken = 0;
    //static puzzle_input = [8,13,1,0,18,9]
    static int puzzle_input[] = {0, 3, 6};
    static int end_turn = 10;
    static int history[][] = new int[end_turn][2];

    public static void update_history(int n) {
        int a[] = history[n];
        if (a[0] == -1)
            history[n] = new int[]{turn, -1};
        else
            history[n] = new int[]{turn, a[0]};
    }

    public static void main(String args[]) {
        for (int i = 0; i < end_turn; i++)
            history[i] = new int[]{-1, -1};

        for (int n : puzzle_input) {
            update_history(n);
            last_spoken = n;
            System.out.println(last_spoken);
            turn++;
        }

        while (turn < end_turn) {
            int h[] = history[last_spoken];
            int next_spoken = (h[0] != -1 ? h[0] - h[1] : 0);
            last_spoken = next_spoken;
            update_history(last_spoken);
            System.out.println(last_spoken);
            turn++;
        }

        //System.out.println(last_spoken);
    }
}
