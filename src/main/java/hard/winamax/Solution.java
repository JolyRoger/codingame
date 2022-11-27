package hard.winamax;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

import static java.lang.Math.max;
import static java.lang.Math.min;

public class Solution {

//    private static File f = new File("resources/winamax/test6.txt");
    private static File f = new File("resources/winamax/test9.txt");

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

    static Set<Integer> stateSet = new HashSet<>();

    static class Point {
        final int x;
        final int y;
        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }
        @Override
        public String toString() {
            return "<" + x + "," + y + '>';
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Point point = (Point) o;
            return x == point.x && y == point.y;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y);
        }
    }
    static class State {
        State(int[][] newState, State from) {
            this.matrix = newState;
            this.from = from;
        }
        private final int[][] matrix;
        private final State from;

        public List<Move> getPossibleMoves() {
            return findMoves(this);
        }
        public State doMove(Move move) {
            List<Point> path = move.getPath();
            Point first = path.get(0);
            Point last = path.get(path.size() - 1);
            int shots = matrix[first.y][first.x];
            int direction = 0;
            if (first.x > last.x) direction = DISPOSED_LEFT;
            if (first.x < last.x) direction = DISPOSED_RIGHT;
            if (first.y < last.y) direction = DISPOSED_DOWN;
            if (first.y > last.y) direction = DISPOSED_UP;

            int[][] copy = new int[matrix.length][];
            for (int i = 0; i < matrix.length; i++) {
                copy[i] = new int[matrix[i].length];
                System.arraycopy(matrix[i], 0, copy[i], 0, matrix[i].length);
            }
            for (Point point : path) {
                copy[point.y][point.x] = direction;
            }
            copy[last.y][last.x] = matrix[last.y][last.x] == HOLE ? DISPOSED_HOLE : shots - 1;
            return new State(copy, this);
        }

        public boolean found(int ballCount) {
            int sum = 0;
            for (int i = 0; i < matrix[0].length; i++) {
                for (int j = 0; j < matrix.length; j++) {
                    if (matrix[j][i] == DISPOSED_HOLE) {
                        sum++;
                    }
                }
            }
            return sum == ballCount;
        }
/*
        public Optional<Ball> doMove(Move move) {
            if (matrix[move.from.y][move.from.x] <= EMPTY) return Optional.empty();
            List<Point> path = move.getPath();
            Point first = path.get(0);
            Point last = path.get(path.size() - 1);
            int shots = matrix[first.y][first.x];
            int direction = 0;
            if (first.x > last.x) direction = DISPOSED_LEFT;
            if (first.x < last.x) direction = DISPOSED_RIGHT;
            if (first.y < last.y) direction = DISPOSED_DOWN;
            if (first.y > last.y) direction = DISPOSED_UP;

            int[][] copy = new int[matrix.length][];
            for (int i = 0; i < matrix.length; i++) {
                copy[i] = new int[matrix[i].length];
                System.arraycopy(matrix[i], 0, copy[i], 0, matrix[i].length);
            }
            for (Point point : path) {
                copy[point.y][point.x] = direction;
            }
            copy[last.y][last.x] = matrix[last.y][last.x] == HOLE ? DISPOSED_HOLE : shots - 1;
            boolean wasted = copy[last.y][last.x] == DISPOSED_HOLE || copy[last.y][last.x] == EMPTY;
//            point._2 * width + point._1 % width
            return Optional.of(new Ball(copy[last.y][last.x], last, new State(copy, this), wasted));
        }
*/

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            State state = (State) o;
            return Arrays.deepEquals(matrix, state.matrix);
        }

        @Override
        public int hashCode() {
            return Arrays.deepHashCode(matrix);
        }
    }
    static class Move {
        Move(Point from, Point to, State state) {
            this.from = from;
            this.to = to;
            this.state = state;
        }
        final Point from;
        final Point to;
        final State state;
        public List<Point> getPath() {
            List<Point> points = new ArrayList<>();
            int xmax = max(from.x, to.x);
            int xmin = min(from.x, to.x);
            int ymax = max(from.y, to.y);
            int ymin = min(from.y, to.y);
            if (xmax > xmin && ymax > ymin) throw new IllegalArgumentException("Diagonal path " + from + "->" + to + " forbidden");
            if (xmax > xmin) {
                if (from.x > to.x) {
                    for (int i = xmax; i >= xmin; i--) points.add(new Point(i, ymin));
                } else {
                    for (int i = xmin; i <= xmax; i++) points.add(new Point(i, ymin));
                }
            }
            if (ymax > ymin) {
                if (from.y > to.y) {
                    for (int i = ymax; i >= ymin; i--) points.add(new Point(xmin, i));
                } else {
                    for (int i = ymin; i <= ymax; i++) points.add(new Point(xmin, i));
                }
            }
            return points;
        }

        @Override
        public String toString() {
            return "Move{from=" + from + ", to=" + to + '}';
        }
    }
    static class Ball {
        Ball(int shots, Point position, State state, boolean wasted) {
            this.shots = shots;
            this.position = position;
            this.state = state;
            this.wasted = wasted;
        }
        final int shots;
        final Point position;
        final State state;
        boolean wasted;

        public List<Move> getPossibleMoves() {
            return findMoves(state, position, shots);
        }

        @Override
        public String toString() {
            return "Ball{shots=" + shots + ", position=" + position + '}';
        }
    }

    private static void findPath(State state, Ball ball) {
        List<Move> path = new ArrayList<>();
        List<Move> stack = new ArrayList<>();
//        for (int i = 0; i < moves.size(); i++) {
//            Move m = moves.get(i);
//        }
    }

    private static void printChar(int[][] state, int width, int height) {
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                System.err.print(toChar.getOrDefault(state[i][j], Character.forDigit(state[i][j], 10)));
            }
            System.err.println();
        }
    }

    private static void printTarget(int[][] state, int width, int height) {
        for (int i = 0; i < height; i++) {
            char[] row = new char[width];
            for (int j = 0; j < width; j++) {
                row[j] = targetMap.getOrDefault(state[i][j], '.');
            }
            System.out.println(new String(row));
        }
    }

    private static void print(int[][] state, int width, int height) {
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                System.err.print(state[i][j] + "\t");
//                System.err.println("state[" + i + "],[" + j + "]=" + state[i][j]);
            }
            System.err.println();
        }
    }

    private static boolean cannotRoll(int[][] matrix, Point point) {
        return matrix[point.y][point.x] != EMPTY && matrix[point.y][point.x] != WATER;
    }

    private static boolean canStop(int[][] matrix, Point point) {
        return matrix[point.y][point.x] == EMPTY || matrix[point.y][point.x] == HOLE;
    }

    private static List<Move> findMoves(State state) {
        int[][] matrix = state.matrix;
        List<Move> moves = new ArrayList<>();
        for (int i = 0; i < matrix[0].length; i++) {
            for (int j = 0; j < matrix.length; j++) {
                if (matrix[j][i] > 0) {
                    moves.addAll(findMoves(state, new Point(i, j), matrix[j][i]));
                }
            }
        }
        return moves;
    }

    private static List<Move> findMoves(State state, Point p, int shots) {
        int[][] matrix = state.matrix;
//        Point p = ball.position;
        if (matrix[p.y][p.x] <= 0) throw new IllegalArgumentException("Wrong point! <" + p.x + "," + p.y + ">");
        List<Move> moves = new ArrayList<>(4);
        if (p.x >= shots) {
            boolean correctly = true;
            for (int i = p.x - 1; i > p.x - shots; i--) {
//                System.err.println("p.y=" + p.y + " i=" + i + " state.matrix[p.y][i]=" + state.matrix[p.y][i]);
                if (cannotRoll(matrix, new Point(i, p.y))) correctly = false;
            }
//            System.err.println("correctly=" + correctly);
            Point target = new Point(p.x - shots, p.y);
            if (correctly && canStop(matrix, target)) {
                moves.add(new Move(p, target, state));
            }
        }
        if (p.x < matrix[0].length - shots) {
            boolean correctly = true;
            for (int i = p.x + 1; i < p.x + shots; i++) {
//                System.err.println("p.y=" + p.y + " i=" + i + " state.matrix[p.y][i]=" + state.matrix[p.y][i]);
                if (cannotRoll(matrix, new Point(i, p.y))) correctly = false;
//                if (state.matrix[p.y][i] != EMPTY && state.matrix[p.y][i] != WATER) correctly = false;
            }
            Point target = new Point(p.x + shots, p.y);
            if (correctly && canStop(matrix, target)) {
                moves.add(new Move(p, target, state));
            }
        }
        if (p.y >= shots) {
            boolean correctly = true;
            for (int i = p.y - 1; i > p.y - shots; i--) {
//                System.err.println("p.y=" + p.y + " i=" + i + " state.matrix[p.y][i]=" + state.matrix[p.y][i]);
                if (cannotRoll(matrix, new Point(p.x, i))) correctly = false;
            }
//            System.err.println("correctly=" + correctly);
            Point target = new Point(p.x, p.y - shots);
            if (correctly && canStop(matrix, target)) {
                moves.add(new Move(p, target, state));
            }
        }
        if (p.y < matrix.length - shots) {
            boolean correctly = true;
            for (int i = p.y + 1; i < p.y + shots; i++) {
//                System.err.println("p.y=" + p.y + " i=" + i + " state.matrix[p.y][i]=" + state.matrix[p.y][i]);
                if (cannotRoll(matrix, new Point(p.x, i))) correctly = false;
            }
//            System.err.println("correctly=" + correctly);
            Point target = new Point(p.x, p.y + shots);
            if (correctly && canStop(matrix, target)) {
                moves.add(new Move(p, target, state));
            }
        }

        return moves;
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
        int[][] stateMatrix = new int[height][width];
        State init = new State(stateMatrix, null);
        List<Ball> balls = new ArrayList<>();
        int ballCount = 0;
        System.err.println(width + " " + height);

        for (int i = 0; i < height; i++) {
            String row = in.next();
            for (int j = 0; j < width; j++) {
                char sym = row.charAt(j);
                stateMatrix[i][j] = stateMap.getOrDefault(sym, Character.getNumericValue(sym));
                if (stateMatrix[i][j] > 0) {
//                    Point p = new Point(j, i);
                    ballCount++;
//                    balls.add(new Ball(stateMatrix[i][j], p, init, false));
                }
            }
            System.err.println(row);
        }
        System.err.println();
//
//        private void calcBalls() {
//            for (int i = 0; i < matrix.length; i++)
//                for (int j = 0; j < matrix[0].length; j++)
//                    if (matrix[i][j] > 0) {
//                        Point p = new Point(i, j);
//                        balls.add(new Ball(matrix[i][j], p, findMoves(matrix, p, matrix[i][j])));
//                    }
//            balls.sort(Comparator.comparingInt(ball -> ball.moves.size()));
//        }

//        init = new State(stateMatrix, null);
//        System.err.println("balls count is " + balls.size());

        Deque<State> stack = new ArrayDeque<>();
        stack.add(init);
        System.err.println("init state size: " + stack.size());

        done : while (!stack.isEmpty()) {
            State state = stack.poll();
            int stateHash = state.hashCode();
            List<Move> moves = new ArrayList<>();
            if (stateSet.contains(stateHash)) {
//                System.err.println("Contains hash " + stateHash);
            } else {
                stateSet.add(stateHash);
                moves = state.getPossibleMoves();
            }
//            List<Move> moves = ball.getPossibleMoves();
            for (Move move : moves) {
                State newState = move.state.doMove(move);
                stack.push(newState);
//                        .ifPresent(newBall -> {
//                    if (newBall.wasted)
                if (newState.found(ballCount)) {
                    printTarget(newState.matrix, width, height);
                    break done;
                }
//                    else
//                });
        }
//            State nextState = move.state.doMove(move);
//            for (Move nextMove : nextState.getPossibleMoves()) {
//                stack.push(nextMove);
//            }
//            printChar(nextState.matrix, width, height);
        }

//        for (Ball ball : balls) {
//            System.err.println(ball + " has " + findMoves(init, ball.position, ball.shots).size() + " moves");
//
//            for (Move move : stack) {
//                System.err.println("\t" + move);
//                State newState = init.doMove(move);
//                printChar(newState.matrix, width, height);
//            }
//        }


        // System.out.println("v..");
        // System.out.println("v..");
        // System.out.println(">.^");

//        System.out.println("v....");
//        System.out.println("v...<");
//        System.out.println("v^..^");
//        System.out.println("v^.^^");
//        System.out.println(">>>^.");
    }
}
