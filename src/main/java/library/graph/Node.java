package library.graph;

import java.util.HashSet;
import java.util.Set;

public class Node<N, E extends Comparable<E>> {

	private N value;
	private Set<Edge<E, N>> edges;

	public Node(N value, Set<Edge<E, N>> edges) {
		this.value = value;
		this.edges = edges;
	}

	public Node(N value) {
		this.value = value;
		this.edges = new HashSet<>();
	}

	public N getValue() {
		return value;
	}

	public void setValue(N value) {
		this.value = value;
	}

	public Set<Edge<E, N>> getEdges() {
		return edges;
	}

	public void setEdges(Set<Edge<E, N>> edges) {
		this.edges = edges;
	}

}
