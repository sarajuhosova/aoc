package aoc2020.day18;

import library.Year;
import library.io.Input;

import java.util.*;
import java.util.stream.Collectors;

public class BasicMath {

    static Map<Character, Operator> operatorMap = Arrays.stream(Operator.values())
            .collect(Collectors.toMap(k -> k.c, k -> k));

    private static Map<Integer, Integer> findParentheses(String s) {
        Map<Integer, Integer> map = new HashMap<>();

        int count = 0;
        int current = -1;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '(') {
                if (count == 0) current = i;
                count++;
            } else if (c == ')') {
                if (count == 1) map.put(current, i + 1);
                count--;
            }
        }

        return map;
    }

    public static Node createTreeWithNoPrecedence(String expression) {
        Map<Integer, Integer> parentheses = findParentheses(expression);

        List<Integer> indices = parentheses.keySet().stream().sorted().collect(Collectors.toList());
        List<Node> subNodes = indices.stream()
                .map(i -> createTreeWithNoPrecedence(expression.substring(i + 1, parentheses.get(i) - 1)))
                .collect(Collectors.toList());

        Map<Integer, Integer> map = new HashMap<>();
        int index = 0;
        int i = 0;
        while (index < expression.length()) {
            if (i >= indices.size()) {
                map.put(index, expression.length());
                break;
            }
            else {
                int ii = indices.get(i);
                if (ii != index) {
                    map.put(index, ii);
                }
                index = parentheses.get(ii);
                i++;
            }
        }

        List<List<Node>> nodes = map.keySet().stream().sorted()
                .map(s -> expression.substring(s, map.get(s)).split(" "))
                .map(a -> Arrays.stream(a)
                        .filter(s -> !s.equals(""))
                        .map(n -> {
                            try {
                                int num = Integer.parseInt(n);
                                return new NumberNode(num);
                            } catch (NumberFormatException nfe) {
                                return new OperatorNode(operatorMap.get(n.charAt(0)));
                            }
                        }).collect(Collectors.toList()))
                .collect(Collectors.toList());

        Queue<Node> q = new LinkedList<>();
        index = 0;
        int nodeI = 0;
        int subI = 0;
        while (index < expression.length()) {
            if (parentheses.containsKey(index)) {
                q.add(subNodes.get(subI));
                subI++;
                index = parentheses.get(index);
            } else {
                q.addAll(nodes.get(nodeI));
                nodeI++;
                index = map.get(index);
            }
        }

        Node current = q.poll();
        while (!q.isEmpty()) {
            Node parent = q.poll();
            Node right = q.poll();

            parent.setLeft(current);
            parent.setRight(right);

            current = parent;
        }

        return current;
    }

    private static long noOperatorPrecedence(List<String> expressions) {
        long total = 0;
        for (String expr : expressions) {
            total += Node.solveTree(createTreeWithNoPrecedence(expr));
        }
        return total;
    }

    public static Node createTreeWithPrecedence(String expression) {
        Map<Integer, Integer> parentheses = findParentheses(expression);

        List<Integer> indices = parentheses.keySet().stream().sorted().collect(Collectors.toList());
        List<Node> subNodes = indices.stream()
                .map(i -> createTreeWithPrecedence(expression.substring(i + 1, parentheses.get(i) - 1)))
                .collect(Collectors.toList());

        Map<Integer, Integer> map = new HashMap<>();
        int index = 0;
        int i = 0;
        while (index < expression.length()) {
            if (i >= indices.size()) {
                map.put(index, expression.length());
                break;
            }
            else {
                int ii = indices.get(i);
                if (ii != index) {
                    map.put(index, ii);
                }
                index = parentheses.get(ii);
                i++;
            }
        }

        List<List<Node>> nodes = map.keySet().stream().sorted()
                .map(s -> expression.substring(s, map.get(s)).split(" "))
                .map(a -> Arrays.stream(a)
                        .filter(s -> !s.equals(""))
                        .map(n -> {
                            try {
                                int num = Integer.parseInt(n);
                                return new NumberNode(num);
                            } catch (NumberFormatException nfe) {
                                return new OperatorNode(operatorMap.get(n.charAt(0)));
                            }
                        }).collect(Collectors.toList()))
                .collect(Collectors.toList());

        Queue<Node> q = new LinkedList<>();
        index = 0;
        int nodeI = 0;
        int subI = 0;
        while (index < expression.length()) {
            if (parentheses.containsKey(index)) {
                q.add(subNodes.get(subI));
                subI++;
                index = parentheses.get(index);
            } else {
                q.addAll(nodes.get(nodeI));
                nodeI++;
                index = map.get(index);
            }
        }

        Deque<Node> dq = new LinkedList<>();
        dq.addLast(q.poll());
        while (!q.isEmpty()) {
            Node parent = q.poll();
            if (parent instanceof OperatorNode) {
                OperatorNode operator = (OperatorNode) parent;
                if (operator.getOperator() == Operator.MULTIPLICATION) {
                    dq.addLast(parent);
                    dq.addLast(q.poll());
                } else {
                    parent.setLeft(dq.removeLast());
                    parent.setRight(q.poll());
                    dq.addLast(parent);
                }
            }
        }

        Node current = dq.poll();
        while (!dq.isEmpty()) {
            Node parent = dq.poll();
            Node right = dq.poll();

            parent.setLeft(current);
            parent.setRight(right);

            current = parent;
        }

        return current;
    }

    private static long withOperatorPrecedence(List<String> expressions) {
        long total = 0;
        for (String expr : expressions) {
            total += Node.solveTree(createTreeWithPrecedence(expr));
        }
        return total;
    }

    public static void main(String[] args) {
        List<String> expressions = Input.readData(Year._2020, "day18.txt")
                .collect(Collectors.toList());

        System.out.println(noOperatorPrecedence(expressions));
        System.out.println(withOperatorPrecedence(expressions));
    }

}
