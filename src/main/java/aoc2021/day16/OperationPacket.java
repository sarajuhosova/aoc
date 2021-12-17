package aoc2021.day16;

import library.tuple.Pair;

import java.util.ArrayList;
import java.util.List;

public class OperationPacket extends Packet {

    private final Operation operation;
    private final List<Packet> packets;

    public OperationPacket(int version, int operator, List<Packet> packets) {
        super(version);
        this.operation = Operation.getFromInt(operator);
        this.packets = packets;
    }

    public List<Packet> getPackets() {
        return packets;
    }

    @Override
    public long evaluate() {
        if (packets.size() == 2) {
            long left = packets.get(0).evaluate();
            long right = packets.get(1).evaluate();
            return operation.getOperation().apply(left, right);
        }
        long start = operation.getStart();
        for (Packet packet : packets) {
            start = operation.getOperation().apply(start, packet.evaluate());
        }
        return start;
    }

    @Override
    public int sumVersions() {
        int sum = getVersion();
        for (Packet sub : packets) {
            sum += sub.sumVersions();
        }
        return sum;
    }

    public static Pair<Packet, String> read(String packet) {
        int version = Integer.parseInt(packet.substring(0, 3), 2);
        int operator = Integer.parseInt(packet.substring(3, 6), 2);

        List<Packet> packets = new ArrayList<>();
        if (packet.charAt(6) == '0') {
            // total length of bits
            int bits = Integer.parseInt(packet.substring(7, 22), 2);

            packet = packet.substring(22);
            int length = packet.length();
            while (packet.length() > (length - bits)) {
                Pair<Packet, String> pair = Packet.read(packet);
                packets.add(pair.getFirst());
                packet = pair.getSecond();
            }
        } else {
            // number of sub-packets
            int num = Integer.parseInt(packet.substring(7, 18), 2);

            packet = packet.substring(18);
            for (int i = 0; i < num; i++) {
                Pair<Packet, String> pair = Packet.read(packet);
                packets.add(pair.getFirst());
                packet = pair.getSecond();
            }
        }

        return new Pair<>(new OperationPacket(version, operator, packets), packet);
    }

}
