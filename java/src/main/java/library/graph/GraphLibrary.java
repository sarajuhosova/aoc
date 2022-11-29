package library.graph;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class GraphLibrary {

	static class DijkstraNode<N> implements Comparable<DijkstraNode<N>> {
		Node<N, Integer> node;
		Integer distance;

		public DijkstraNode(Node<N, Integer> node, Integer distance) {
			this.node = node;
			this.distance = distance;
		}

		@Override
		public int compareTo(DijkstraNode<N> o) {
			return this.distance.compareTo(o.distance);
		}

		@Override
		public boolean equals(Object o) {
			if (this == o)
				return true;
			if (o == null || getClass() != o.getClass())
				return false;

			DijkstraNode<?> that = (DijkstraNode<?>) o;

			return node != null ? node.equals(that.node) : that.node == null;
		}
	}

	public static <N> List<Node<N, Integer>> shortestDirectedPath(Node<N, Integer> source,
			Node<N, Integer> sink) {
		Map<Node<N, Integer>, Node<N, Integer>> predecessors = new HashMap<>();
		Map<Node<N, Integer>, Integer> map = new HashMap<>();
		Set<Node<N, Integer>> visited = new HashSet<>();

		map.put(source, 0);
		predecessors.put(source, null);

		Queue<DijkstraNode<N>> q = new PriorityQueue<>();
		q.add(new DijkstraNode<>(source, 0));

		while (!q.isEmpty()) {
			DijkstraNode<N> current = q.remove();
			visited.add(current.node);
			if (current.node == sink) {
				break;
			}
			for (Edge<Integer, N> e : current.node.getEdges()) {
				if (!visited.contains(e.getNode())) {
					Integer distance = map.getOrDefault(current.node, Integer.MAX_VALUE);
					Integer newDistance = map.get(current.node) + e.getWeight();
					if (distance <= newDistance) {
						q.remove(new DijkstraNode<>(e.getNode(), distance));
						map.put(e.getNode(), newDistance);
						predecessors.put(e.getNode(), current.node);
						q.add(new DijkstraNode<>(e.getNode(), newDistance));
					}
				}
			}
		}

		return visited.contains(sink) ? Stream
				.iterate(sink, Objects::isNull, predecessors::get)
				.collect(Collectors.toList()) : null;
	}

}
