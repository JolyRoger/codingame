package codingames.hard.winamax;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;

import static java.lang.Math.max;
import static java.lang.Math.min;

public class Solution {

    private static File f = new File("resources/winamax/test20.txt");

    private static final int EMPTY = 0;
    private static final int WATER = -1;
    private static final int HOLE = -2;
    private static final int DISPOSED_HOLE = -3;
    private static final int DISPOSED_DOWN = -4;
    private static final int DISPOSED_LEFT = -5;
    private static final int DISPOSED_UP = -6;
    private static final int DISPOSED_RIGHT = -7;

    protected static final Map<Integer, Character> toChar = new HashMap<>();

    static {
        toChar.put(EMPTY, '.');
        toChar.put(WATER, 'X');
        toChar.put(HOLE, 'H');
        toChar.put(DISPOSED_HOLE, 'o');
        toChar.put(DISPOSED_DOWN, 'v');
        toChar.put(DISPOSED_LEFT, '<');
        toChar.put(DISPOSED_RIGHT, '>');
        toChar.put(DISPOSED_UP, '^');
    }

    protected static final Map<Integer, Character> targetMap = new HashMap<>();

    static {
        targetMap.put(EMPTY, '.');
        targetMap.put(DISPOSED_DOWN, 'v');
        targetMap.put(DISPOSED_LEFT, '<');
        targetMap.put(DISPOSED_RIGHT, '>');
        targetMap.put(DISPOSED_UP, '^');
    }

    static int width;
    static int height;

    static int toX(int number) {
        return number % width;
    }
    static int toY(int number) {
        return number / width;
    }
    static int toNumber(int x, int y) {
        return y * width + x % width;
    }

    static class Path {
        Path(List<Move> moves) {
            this.moves = moves;
        }
        private final List<Move> moves;
    }
    static class Ball {
        Ball(int pos, int shots) {
            this.shots = shots;
            this.pos = pos;
        }
        private final int shots;
        private final int pos;
        Map<Integer, Path> paths = new TreeMap<>();

        public Node find(Set<Integer> wasted, Set<Integer> hashes, int[] selected, int ballIndex, State state) {
            int vertex;
            int attemptSelectedHash;
            State newState = null;
            int[] attemptSelected = selected.clone();
            Iterator<Integer> it = paths.keySet().iterator();
            do {
                vertex = it.next();
                newState = state.applyPath(paths.get(vertex));
                attemptSelected[ballIndex] = vertex;
                attemptSelectedHash = Arrays.hashCode(attemptSelected);
            } while (it.hasNext() && (wasted.contains(vertex) || hashes.contains(attemptSelectedHash) || newState == null));

            if (wasted.contains(vertex) || hashes.contains(attemptSelectedHash) || newState == null) return null;
            else return new Node(vertex, newState);
        }
    }

    static class Node {
        Node(int vertex, State state) {
            this.vertex = vertex;
            this.state = state;
        }
        int vertex;
        State state;
    }

    static class State {
        State(int[] newState) {
            this.matrix = newState;
        }
        private final int[] matrix;

        public State applyPath(Path path) {
            State newState = this;
            for (Move move : path.moves) {
                newState = newState.doMove(move);
                if (newState == null) return null;
            }
            return newState;
        }
        public State doMove(Move move) {
            List<Integer> path = move.getPath();
            if (forbidden(path)) return null;

            int first = path.get(0);
            int last = path.get(path.size() - 1);
            int shots = matrix[first];
            int direction = 0;
            int firstX = toX(first);
            int firstY = toY(first);
            int lastX = toX(last);
            int lastY = toY(last);
            if (firstX > lastX) direction = DISPOSED_LEFT;
            if (firstX < lastX) direction = DISPOSED_RIGHT;
            if (firstY < lastY) direction = DISPOSED_DOWN;
            if (firstY > lastY) direction = DISPOSED_UP;

            int[] copy = matrix.clone();
            for (int point : path) {
                copy[point] = direction;
            }
            copy[last] = matrix[last] == HOLE ? DISPOSED_HOLE : shots - 1;
            return new State(copy);
        }

        private boolean forbidden(List<Integer> path) {
            for (Integer vertex : path) {
                if (matrix[vertex] == DISPOSED_LEFT ||
                    matrix[vertex] == DISPOSED_RIGHT ||
                    matrix[vertex] == DISPOSED_DOWN ||
                    matrix[vertex] == DISPOSED_UP) return true;
            }
            return false;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            State state = (State) o;
            return Arrays.equals(matrix, state.matrix);
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(matrix);
        }

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            for (int i = 0; i < matrix.length; i++) {
                if (i % width == 0) builder.append("\n");
                builder.append(toChar.getOrDefault(matrix[i], Character.forDigit(matrix[i], 10)));
            }
            return builder.toString();
        }
    }
    static class Move {
        Move(int from, int to, State state, boolean inHole, Move prevMove) {
            this.from = from;
            this.to = to;
            this.state = state;
            this.inHole = inHole;
            this.prevMove = prevMove;
        }
        final int from;
        final int to;
        final State state;
        final boolean inHole;
        final Move prevMove;

        public List<Integer> getPath() {
            List<Integer> points = new ArrayList<>();
            int fromX = toX(from);
            int fromY = toY(from);
            int toX = toX(to);
            int toY = toY(to);
            int xmax = max(fromX, toX);
            int xmin = min(fromX, toX);
            int ymax = max(fromY, toY);
            int ymin = min(fromY, toY);
            if (xmax > xmin && ymax > ymin) throw new IllegalArgumentException("Diagonal path " + from + "->" + to + " forbidden");
            if (xmax > xmin) {
                if (fromX > toX) {
                    for (int i = xmax; i >= xmin; i--) points.add(toNumber(i, ymin));
                } else {
                    for (int i = xmin; i <= xmax; i++) points.add(toNumber(i, ymin));
                }
            }
            if (ymax > ymin) {
                if (fromY > toY) {
                    for (int i = ymax; i >= ymin; i--) points.add(toNumber(xmin, i));
                } else {
                    for (int i = ymin; i <= ymax; i++) points.add(toNumber(xmin, i));
                }
            }
            return points;
        }

        @Override
        public String toString() {
            return "Move{from=" + from + ", to=" + to + '}';
        }
    }

    private static void printTarget(int[] state, int width, int height) {
        for (int i = 0; i < height; i++) {
            char[] row = new char[width];
            for (int j = 0; j < width; j++) {
                row[j] = targetMap.getOrDefault(state[toNumber(j, i)], '.');
            }
            System.out.println(new String(row));
        }
    }

    private static boolean cannotRoll(int[] matrix, int point) {
        return matrix[point] != EMPTY && matrix[point] != WATER;
    }

    private static int canStop(int[] matrix, int point) {
        return matrix[point] == HOLE ? 2 : matrix[point] == EMPTY ? 1 : 0;
    }

    private static List<Move> findMoves(State state, int p, int shots, Move prevMove) {
        if (shots == 0 || state.matrix[p] <= 0) return List.of();
        int[] matrix = state.matrix;
        int px = toX(p);
        int py = toY(p);

        List<Move> moves = new ArrayList<>(4);
        if (px >= shots) {
            boolean correctly = true;
            int target = toNumber(px - shots, py);
            for (int i = px - 1; i > px - shots; i--) {
                if (cannotRoll(matrix, toNumber(i, py))) {
                    correctly = false;
                    break;
                }
            }
            int stopped = canStop(matrix, target);
            if (correctly && stopped > 0) {
                moves.add(new Move(p, target, state, stopped == 2, prevMove));
            }
        }
        if (px < width - shots) {
            boolean correctly = true;
            int target = toNumber(px + shots, py);
            for (int i = px + 1; i < px + shots; i++) {
                int currentNum = toNumber(i, py);
                if (cannotRoll(matrix, currentNum)) {
                    correctly = false;
                    break;
                }
            }
            int stopped = canStop(matrix, target);
            if (correctly && stopped > 0) {
                moves.add(new Move(p, target, state, stopped == 2, prevMove));
            }
        }
        if (py >= shots) {
            boolean correctly = true;
            int target = toNumber(px, py - shots);
            for (int i = py - 1; i > py - shots; i--) {
                if (cannotRoll(matrix, toNumber(px, i))) {
                    correctly = false;
                    break;
                }
            }
            int stopped = canStop(matrix, target);
            if (correctly && stopped > 0) {
                moves.add(new Move(p, target, state, stopped == 2, prevMove));
            }
        }
        if (py < height - shots) {
            boolean correctly = true;
            int target = toNumber(px, py + shots);
            for (int i = py + 1; i < py + shots; i++) {
                if (cannotRoll(matrix, toNumber(px, i))) {
                    correctly = false;
                    break;
                }
            }
            int stopped = canStop(matrix, target);
            if (correctly && stopped > 0) {
                moves.add(new Move(p, target, state, stopped == 2, prevMove));
            }
        }

        return moves;
    }

    private static Set<Integer> allValues(int[] arr) {
        Set<Integer> out = new HashSet<>(arr.length);
        for (int v: arr) {
            if (v == -1) break;
            out.add(v);
        }
        return out;
    }

    public static void main(String args[]) throws FileNotFoundException {
//        Scanner in = new Scanner(System.in);
        Scanner in = new Scanner(f);

        Map<Character, Integer> stateMap = new HashMap<>(3);
        stateMap.put('.', EMPTY);
        stateMap.put('X', WATER);
        stateMap.put('H', HOLE);

        width = in.nextInt();
        height = in.nextInt();
        int size = height * width;
        int[] stateMatrix = new int[size];

        State init = new State(stateMatrix);

        for (int i = 0; i < height; i++) {
            String row = in.next();
            for (int j = 0; j < width; j++) {
                char sym = row.charAt(j);
                int index = toNumber(j, i);
                stateMatrix[index] = stateMap.getOrDefault(sym, Character.getNumericValue(sym));
            }
        }

        List<Ball> balls = new ArrayList<>();
        for (int i = 0; i < init.matrix.length; i++) {
            if (init.matrix[i] > 0) balls.add(new Ball(i, init.matrix[i]));
        }

        for (Ball ball : balls) {
            Deque<Move> moves = new ArrayDeque<>(findMoves(init, ball.pos, ball.shots, null));
            while (!moves.isEmpty()) {
                Move move = moves.poll();
                if (move.inHole) {
                    Deque<Move> path = new ArrayDeque<>();
                    Move currentMove = move;
                    while (currentMove != null) {
                        path.push(currentMove);
                        currentMove = currentMove.prevMove;
                    }
                    ball.paths.put(move.to, new Path(new ArrayList<>(path)));
                } else {
                    List<Integer> path = move.getPath();
                    int last = path.get(path.size() - 1);
                    State newState = move.state.doMove(move);
                    int newShots = newState.matrix[last];
                    List<Move> newMoves = findMoves(newState, last, newShots, move);
                    for (Move newMove : newMoves) {
                        moves.push(newMove);
                    }
                }
            }
        }

        balls.sort(Comparator.comparingInt(b -> b.paths.size()));

        Deque<Node> stack2 = new ArrayDeque<>();
        int ballIndex = 0;
        Set<Integer> wasted = new HashSet<>();
        Set<Integer> hashes = new HashSet<>();
        int[] selected = new int[balls.size()];
        Arrays.fill(selected, -1);

        while(ballIndex < balls.size()) {
            State currentState = Optional.ofNullable(stack2.peek()).map(node -> node.state).orElse(init);
            Ball ball = balls.get(ballIndex);
            Node node = ball.find(wasted, hashes, selected, ballIndex, currentState);

            if (node == null) {
                stack2.poll();
                selected[ballIndex] = -1;
                ballIndex--;
            } else {
                selected[ballIndex] = node.vertex;
                stack2.push(node);
                ballIndex++;
            }
            wasted.clear();
            wasted.addAll(allValues(selected));
            hashes.add(Arrays.hashCode(selected));
        }

        printTarget(stack2.getFirst().state.matrix, width, height);
    }
}
