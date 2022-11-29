package aoc2021.day16;

import library.tuple.Pair;

public class LiteralPacket extends Packet {

    private final long value;

    public LiteralPacket(int version, long value) {
        super(version);
        this.value = value;
    }

    @Override
    public long evaluate() {
        return value;
    }

    @Override
    public int sumVersions() {
        return getVersion();
    }

    public static Pair<Packet, String> read(String string) {
        int version = Integer.parseInt(string.substring(0, 3), 2);

        int i = 6;
        String packet = "";
        boolean zero = false;
        while (!zero) {
            if (string.charAt(i) == '0') zero = true;
            packet += string.substring(i + 1, i + 5);
            i += 5;
        }

        string = string.substring(6 + ((packet.length() / 4) * 5));
        return new Pair<>(new LiteralPacket(version, Long.parseLong(packet, 2)), string);
    }

    @Override
    protected void prettyPrint(int depth) {
        super.prettyPrint(depth);
        System.out.println(value);
    }

}
