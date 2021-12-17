package aoc2021.day16;

import java.util.HashMap;
import java.util.Map;

public class Converter {

    private static Map<Character, String> getHexToBin() {
        Map<Character, String> map = new HashMap<>();
        map.put('0', "0000");
        map.put('1', "0001");
        map.put('2', "0010");
        map.put('3', "0011");
        map.put('4', "0100");
        map.put('5', "0101");
        map.put('6', "0110");
        map.put('7', "0111");
        map.put('8', "1000");
        map.put('9', "1001");
        map.put('A', "1010");
        map.put('B', "1011");
        map.put('C', "1100");
        map.put('D', "1101");
        map.put('E', "1110");
        map.put('F', "1111");
        return map;
    }

    public static String read(String hex) {
        Map<Character, String> map = getHexToBin();
        StringBuilder bin = new StringBuilder();
        for (int i = 0; i < hex.length(); i++) {
            bin.append(map.get(hex.charAt(i)));
        }
        return bin.toString();
    }

}
