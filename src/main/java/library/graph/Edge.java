package library.graph;

public class Edge<E extends Comparable<E>, N> implements Comparable<Edge<E, N>> {

	private E weight;
	private Node<N, E> node;

	public Edge(E weight, Node<N, E> node) {
		this.weight = weight;
		this.node = node;
	}

	public E getWeight() {
		return weight;
	}

	public void setWeight(E weight) {
		this.weight = weight;
	}

	public Node<N, E> getNode() {
		return node;
	}

	public void setNode(Node<N, E> node) {
		this.node = node;
	}

	@Override
	public int compareTo(Edge<E, N> o) {
		return this.weight.compareTo(o.weight);
	}
}
