package aoc2021.day05;

class Point {
    private final int x;
    private final int y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public static Point parse(String s) {
        String[] data = s.split(",");
        return new Point(Integer.parseInt(data[0]), Integer.parseInt(data[1]));
    }

    @Override
    public String toString() {
        return "(" + x + ", " + y + ")";
    }

}