package library.tuple;

public class Triple<A, B, C> {

	private A a;
	private B b;
	private C c;

	public Triple(A a, B b, C c) {
		this.a = a;
		this.b = b;
		this.c = c;
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

	public void setFirst(A a) {
		this.a = a;
	}

	public void setSecond(B b) {
		this.b = b;
	}

	public void setThird(C c) {
		this.c = c;
	}
}
