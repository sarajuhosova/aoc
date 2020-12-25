package aoc2020.day25;

import library.Year;
import library.io.Input;

import java.util.Scanner;

public class KeyCardDecryption {

    private static final int MODULATOR = 20201227;

    private static int countLoops(int key, int subject) {
        int loops = 0;
        int calculated = 1;
        while (calculated != key) {
            calculated = (calculated * subject + MODULATOR) % MODULATOR;
            loops++;
        }
        return loops;
    }

    private static long getEncryptionKey(int key, int loops) {
        long calculated = 1;
        for (int i = 0; i < loops; i++) {
            calculated = (calculated * key + MODULATOR) % MODULATOR;
        }
        return (calculated + MODULATOR) % MODULATOR;
    }

    public static void main(String[] args) {
        Scanner sc = Input.openFile(Year.AOC_2020, "day25.txt");
        int cardKey = sc.nextInt();
        int doorKey = sc.nextInt();

        int cardLoops = countLoops(cardKey, 7);
        int doorLoops = countLoops(doorKey, 7);

        System.out.println(getEncryptionKey(doorKey, cardLoops));
        System.out.println(getEncryptionKey(cardKey, doorLoops));
    }

}
