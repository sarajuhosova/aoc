package library.tuple;

public class Quintuple<A, B, C, D, E> extends Tuple {

	private A a;
	private B b;
	private C c;
	private D d;
	private E e;

	public Quintuple(A a, B b, C c, D d, E e) {
		this.a = a;
		this.b = b;
		this.c = c;
		this.d = d;
		this.e = e;
	}

	public A getFirst() {
		return a;
	}

	public B getSecond() {
		return b;
	}

	public C getThird() {
		return c;
	}

	public D getFourth() {
		return d;
	}

	public E getFifth() {
		return e;
	}

	public void setFirst(A a) {
		this.a = a;
	}

	public void setSecond(B b) {
		this.b = b;
	}

	public void setThird(C c) {
		this.c = c;
	}

	public void setFourth(D d) {
		this.d = d;
	}

	public void setFifth(E e) {
		this.e = e;
	}
}
