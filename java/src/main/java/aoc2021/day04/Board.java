package aoc2021.day04;

import java.util.Scanner;

public class Board {

    private int[][] numbers;
    private boolean[][] marked;

    public Board(int[][] numbers) {
        this.numbers = numbers;
        this.marked = new boolean[numbers.length][numbers[0].length];
    }

    public void setNumbers(int number) {
        for (int i = 0; i < numbers.length; i++) {
            for (int j = 0; j < numbers[0].length; j++) {
                if (number == numbers[i][j])
                    marked[i][j] = true;
            }
        }
    }

    private boolean hasHorizontal() {
        for (int i = 0; i < numbers.length; i++) {
            int count = 0;
            for (int j = 0; j < numbers[i].length; j++) {
                if (marked[i][j]) count++;
            }
            if (count == numbers[i].length)
                return true;
        }
        return false;
    }

    private boolean hasVertical() {
        for (int j = 0; j < numbers[0].length; j++) {
            int count = 0;
            for (int i = 0; i < numbers.length; i++) {
                if (marked[i][j]) count++;
            }
            if (count == numbers.length)
                return true;
        }
        return false;
    }

    public boolean won() {
        return hasHorizontal() || hasVertical();
    }

    public int getScore() {
        int sum = 0;
        for (int i = 0; i < numbers.length; i++) {
            for (int j = 0; j < numbers[0].length; j++) {
                if (!marked[i][j]) sum += numbers[i][j];
            }
        }
        return sum;
    }

    public static Board read(Scanner sc, int size) {
        int[][] board = new int[size][size];
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                board[i][j] = sc.nextInt();
            }
        }
        return new Board(board);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (int[] number : numbers) {
            for (int num : number) {
                builder.append(String.format("%2d ", num));
            }
            builder.append("\n");
        }
        return builder.toString();
    }

}
