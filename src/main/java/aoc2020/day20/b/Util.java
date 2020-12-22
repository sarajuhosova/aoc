package aoc2020.day20.b;

import java.util.List;

public class Util {

    public static String reverse(String string) {
        return new StringBuilder(string).reverse().toString();
    }

    public static char[][] rotateLeft(char[][] image) {
        char[][] rotated = new char[image[0].length][image.length];

        for (int i = 0; i < image[0].length; i++) {
            for (int j = 0; j < image.length; j++) {
                rotated[i][j] = image[j][image[0].length - 1 - i];
            }
        }

        return rotated;
    }

    public static char[][] rotateRight(char[][] image) {
        char[][] rotated = new char[image[0].length][image.length];

        for (int i = 0; i < image[0].length; i++) {
            for (int j = 0; j < image.length; j++) {
                rotated[i][j] = image[image.length - j - 1][i];
            }
        }

        return rotated;
    }

    public static char[][] flipHor(char[][] image) {
        char[][] rotated = new char[image.length][image[0].length];

        for (int i = 0; i < image.length; i++) {
            for (int j = 0; j < image[0].length; j++) {
                rotated[i][j] = image[i][image[0].length - 1 - j];
            }
        }

        return rotated;
    }

    public static char[][] flipVert(char[][] image) {
        char[][] rotated = new char[image.length][image[0].length];

        for (int i = 0; i < image.length; i++) {
            for (int j = 0; j < image[0].length; j++) {
                rotated[i][j] = image[image.length - 1 - i][j];
            }
        }

        return rotated;
    }

    public static char[][] getNextOrientation(char[][] image, int count) {
        return switch (count) {
            case 1, 3, 6 -> flipHor(image);
            case 2, 5, 7 -> flipVert(image);
            case 4 -> rotateLeft(image);
            default -> image;
        };
    }

    public static void printImage(char[][] image) {
        for (int i = 0; i < image.length; i++) {
            for (int j = 0; j < image[0].length; j++) {
                System.out.print(image[i][j]);
            }
            System.out.println();
        }
        System.out.println();
    }

    public static void main(String[] args) {
        List<String> data = List.of("......", "..#.#.", ".####.", "......", ".##...", ".#....");
        Tile tile = new Tile(0);
        tile.addImage(data);

        char[][] image = tile.getImage();
        for (int i = 0; i < 8; i++) {
            image = getNextOrientation(image, i);
            printImage(image);
        }
    }
}
