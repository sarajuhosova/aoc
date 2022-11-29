package aoc2021.day05;

import java.util.List;

public class VentMap {

    private final int[][] map;

    public VentMap(List<Line> lines, boolean diagonal) {
        int x_size = getHorizontal(lines);
        int y_size = getVertical(lines);
        this.map = new int[x_size][y_size];

        for (Line line : lines) addLine(line, diagonal);
    }

    private int getHorizontal(List<Line> lines) {
        return lines.stream()
                .mapToInt(l -> Math.max(l.getStart().getX(), l.getEnd().getX()) + 1)
                .max()
                .orElse(Integer.MAX_VALUE);
    }

    private int getVertical(List<Line> lines) {
        return lines.stream()
                .mapToInt(l -> Math.max(l.getStart().getY(), l.getEnd().getY()) + 1)
                .max()
                .orElse(Integer.MAX_VALUE);
    }

    private void addHorizontal(Line line) {
        int start = Math.min(line.getStart().getY(), line.getEnd().getY());
        int end = Math.max(line.getStart().getY(), line.getEnd().getY());

        for (int i = start; i <= end; i++) {
            map[line.getStart().getX()][i]++;
        }
    }

    private void addVertical(Line line) {
        int start = Math.min(line.getStart().getX(), line.getEnd().getX());
        int end = Math.max(line.getStart().getX(), line.getEnd().getX());

        for (int i = start; i <= end; i++) {
            map[i][line.getStart().getY()]++;
        }
    }

    private void addDiagonal(Line line) {
        int s_x = line.getStart().getX();
        int e_x = line.getEnd().getX();
        int s_y = line.getStart().getY();
        int e_y = line.getEnd().getY();

        while (true) {
            map[s_x][s_y]++;

            if (s_x == e_x || s_y == e_y) break;

            s_x = (s_x < e_x) ? s_x + 1 : s_x - 1;
            s_y = (s_y < e_y) ? s_y + 1 : s_y - 1;
        }
    }


    private void addLine(Line line, boolean diagonal) {
        if (line.getStart().getX() == line.getEnd().getX())
            addHorizontal(line);
        else if (line.getStart().getY() == line.getEnd().getY())
            addVertical(line);
        else if (diagonal)
            addDiagonal(line);
    }

    public int countDangerousSpots() {
        int sum = 0;
        for (int[] row : map) {
            for (int vents : row) {
                if (vents > 1) sum++;
            }
        }
        return sum;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (int[] row : map) {
            for (int vents : row) {
                if (vents >= 1) builder.append(String.format("%2d ", vents));
                else builder.append("   ");
            }
            builder.append("\n");
        }
        return builder.toString();
    }

}
