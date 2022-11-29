package aoc2020.day24;

import java.util.*;

public class Hexagon {

	private final int id;
	private boolean flipped = false;
	private Map<Direction, Hexagon> hexagons;

	public Hexagon(int id) {
		this.id = id;
		this.hexagons = new HashMap<>();
	}

	public int getId() {
		return id;
	}

	public boolean isFlipped() {
		return flipped;
	}

	public void flip() {
		flipped = !flipped;
	}

	public Map<Direction, Hexagon> getHexagons() {
		return hexagons;
	}

	public void setHexagons(Map<Direction, Hexagon> hexagons) {
		this.hexagons = hexagons;
	}

	public void connect(Hexagon other, Direction d) {
		hexagons.put(d, other);
		other.hexagons.put(d.getOpposite(), this);
	}

	public static List<Hexagon> buildHoneyComb(int R) {
		List<Hexagon> hexagons = new ArrayList<>();
		for (int i = 0; i < R * R * R - (R - 1) * (R - 1) * (R - 1) + 1; i++) {
			hexagons.add(new Hexagon(i));
		}

		int id = 1;
		int length = R;
		for (int i = 0; i < 2 * R - 1; i++) {
			int nextLength = (i < R - 1) ? length + 1 : length - 1;

			for (int j = 0; j < length; j++) {
				Hexagon h = hexagons.get(id);

				if (j != 0)
					hexagons.get(id - 1).connect(h, Direction.WEST);

				if (length < nextLength) {
					// Top half
					hexagons.get(id + length).connect(h, Direction.SOUTHWEST);
					hexagons.get(id + length + 1).connect(h, Direction.SOUTHEAST);
				} else if (length != 2 * R - 1) {
					// Bottom half, not in the middle row
					hexagons.get(id - length - 1).connect(h, Direction.NORTHWEST);
					hexagons.get(id - length).connect(h, Direction.NORTHEAST);
				}

				id++;
			}

			length = nextLength;
		}

		return hexagons;
	}

}
