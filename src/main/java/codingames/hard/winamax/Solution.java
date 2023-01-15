package codingames.hard.winamax;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

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

    protected static final Map<Integer, Character> toChar = Map.of(
        EMPTY, '.',
        WATER, 'X',
        HOLE, 'H',
        DISPOSED_HOLE, 'o',
        DISPOSED_DOWN, 'v',
        DISPOSED_LEFT, '<',
        DISPOSED_RIGHT, '>',
        DISPOSED_UP, '^'
    );

    protected static final Map<Integer, Character> targetMap = Map.of(
        EMPTY, '.',
        DISPOSED_DOWN, 'v',
        DISPOSED_LEFT, '<',
        DISPOSED_RIGHT, '>',
        DISPOSED_UP, '^'
    );

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

    static class Ball {
        Ball(int pos, int shots) {
            this.shots = shots;
            this.pos = pos;
        }
        private final int shots;
        private final int pos;
        Map<Integer, List<Move>> paths = new TreeMap<>();

        public Optional<Node> find(Set<Integer> wasted, Set<Integer> hashes, int[] selected, int ballIndex, State state) {
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

            if (wasted.contains(vertex) || hashes.contains(attemptSelectedHash) || newState == null) return Optional.empty();
            else return Optional.of(new Node(vertex, newState));
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

        public State applyPath(List<Move> path) {
            State newState = this;
            for (Move move : path) {
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
        return Arrays.stream(arr).filter(v -> v != -1).boxed().collect(Collectors.toSet());
    }

    public static void main(String args[]) throws FileNotFoundException {
//        Scanner in = new Scanner(System.in);
        Scanner in = new Scanner(f);

        Map<Character, Integer> stateMap = Map.of('.', EMPTY, 'X', WATER, 'H', HOLE);

        width = in.nextInt();
        height = in.nextInt();

        int[] stateMatrix = IntStream.range(0, height).flatMap(i -> {
                String row = in.next();
                return IntStream.range(0, width).map(j -> {
                    char sym = row.charAt(j);
                    return stateMap.getOrDefault(sym, Character.getNumericValue(sym));
               });
            }).toArray();

        State init = new State(stateMatrix);

        List<Ball> balls = IntStream.range(0, init.matrix.length)
                .filter(i -> init.matrix[i] > 0)
                .mapToObj(i -> new Ball(i, init.matrix[i]))
                .collect(Collectors.toList());

        balls.forEach(ball -> {
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
                    ball.paths.put(move.to, new ArrayList<>(path));
                } else {
                    List<Integer> path = move.getPath();
                    int last = path.get(path.size() - 1);
                    State newState = move.state.doMove(move);
                    int newShots = newState.matrix[last];
                    findMoves(newState, last, newShots, move).forEach(moves::push);
                }
            }}
        );

        Deque<Node> stack = new ArrayDeque<>();
        Set<Integer> wasted = new HashSet<>();
        Set<Integer> hashes = new HashSet<>();
        final int[] selected = new int[balls.size()];
        final int[] ballIndex = {0};
        Arrays.fill(selected, -1);

        while(ballIndex[0] < balls.size()) {
            State currentState = Optional.ofNullable(stack.peek()).map(node -> node.state).orElse(init);
            Ball ball = balls.get(ballIndex[0]);
            ball.find(wasted, hashes, selected, ballIndex[0], currentState)
                    .ifPresentOrElse(node -> {
                        stack.push(node);
                        selected[ballIndex[0]] = node.vertex;
                        ballIndex[0]++;
                    }, () -> {
                        stack.poll();
                        selected[ballIndex[0]] = -1;
                        ballIndex[0]--;
                    });

            wasted.clear();
            wasted.addAll(allValues(selected));
            hashes.add(Arrays.hashCode(selected));
        }

        printTarget(stack.getFirst().state.matrix, width, height);
    }
}
