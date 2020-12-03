package library.tree;

import java.util.Stack;

public class BinaryTreeIterator<T> {

    Stack<BinaryTree<T>> s = new Stack<>();
    Stack<T> snapshot = new Stack<>();

    public BinaryTreeIterator(BinaryTree<T> root) {
        s.push(root);
        while (!s.isEmpty()) {
            BinaryTree<T> current = s.pop();
            if (current.hasLeft()) s.push(current.getLeft());
            if (current.hasRight()) s.push(current.getRight());
            snapshot.push(current.getValue());
        }
    }

    public T next() {
        if (snapshot.isEmpty()) return null;
        return snapshot.pop();
    }

    public boolean hasNext() {
        return !snapshot.isEmpty();
    }

}