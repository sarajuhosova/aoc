package library.tuple;

public abstract class Tuple {

	public static <A, B> Pair<A, B> of(A a, B b) {
		return new Pair<>(a, b);
	}

	public static <A, B, C> Triple<A, B, C> of(A a, B b, C c) {
		return new Triple<>(a, b, c);
	}

	public static <A, B, C, D> Quadruple<A, B, C, D> of(A a, B b, C c, D d) {
		return new Quadruple<>(a, b, c, d);
	}

	public static <A, B, C, D, E> Quintuple<A, B, C, D, E> of(A a, B b, C c, D d, E e) {
		return new Quintuple<>(a, b, c, d, e);
	}

}
