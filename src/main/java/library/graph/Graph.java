package library.graph;

import java.util.*;

public class Graph<N, E extends Comparable<E>> {

	private Set<Node<N, E>> nodes;

	public Graph(boolean directed, Set<Node<N, E>> nodes) {
		this.nodes = nodes;
	}

	public Graph(boolean directed) {
		this.nodes = new HashSet<>();
	}

	public void addNode(Node<N, E> node) {
		nodes.add(node);
	}

}
