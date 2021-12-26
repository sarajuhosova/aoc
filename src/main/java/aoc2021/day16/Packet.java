package aoc2021.day16;

import library.tuple.Pair;

public abstract class Packet {

    private final int version;

    public Packet(int version) {
        this.version = version;
    }

    public int getVersion() {
        return version;
    }

    public abstract long evaluate();

    public abstract int sumVersions();

    public static Pair<Packet, String> read(String string) {
        String type = string.substring(3, 6);
        if (type.equals("100")) return LiteralPacket.read(string);
        else return OperationPacket.read(string);
    }

    public void prettyPrint() {
        prettyPrint(0);
    }

    protected void prettyPrint(int depth) {
        if (depth > 0) {
            for (int i = 0; i < depth - 1; i++) {
                System.out.print("   ");
            }
            System.out.print("|- ");
        }
    }

}
