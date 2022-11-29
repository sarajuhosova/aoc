package library.honeycomb;

import java.util.*;

public class Hexagon {

	private final int id;
	private Set<Hexagon> hexagons;

	public Hexagon(int id) {
		this.id = id;
		this.hexagons = new HashSet<>();
	}

	public Hexagon(int id, Set<Hexagon> hexagons) {
		this.id = id;
		this.hexagons = hexagons;
	}

	public int getId() {
		return id;
	}

	public Set<Hexagon> getHexagons() {
		return hexagons;
	}

	public void setHexagons(Set<Hexagon> hexagons) {
		this.hexagons = hexagons;
	}

	public void connect(Hexagon other) {
		hexagons.add(other);
		other.hexagons.add(this);
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
					hexagons.get(id - 1).connect(h);

				if (length < nextLength) {
					// Top half
					hexagons.get(id + length).connect(h);
					hexagons.get(id + length + 1).connect(h);
				} else if (length != 2 * R - 1) {
					// Bottom half, not in the middle row
					hexagons.get(id - length - 1).connect(h);
					hexagons.get(id - length).connect(h);
				}

				id++;
			}

			length = nextLength;
		}

		return hexagons;
	}

	public static int bfs(Hexagon a, Hexagon b) {
		return bfs(a, b, Set.of());
	}

	public static int bfs(Hexagon a, Hexagon b, Set<Integer> walls) {
		Queue<Hexagon> q = new LinkedList<>();
		q.add(a);

		Map<Hexagon, Integer> distances = new HashMap<>();
		distances.put(a, 0);

		Set<Hexagon> visited = new HashSet<>();

		while (!q.isEmpty()) {
			Hexagon current = q.poll();
			visited.add(current);

			if (current.id == b.id)
				return distances.get(current);

			for (Hexagon h : current.getHexagons()) {
				if (!visited.contains(h) && !walls.contains(h.id)) {
					distances.put(h,
							Math.min(distances.getOrDefault(h,
									Integer.MAX_VALUE), distances.get(current) + 1));
					q.offer(h);
				}
			}
		}

		return Integer.MAX_VALUE;
	}
}
