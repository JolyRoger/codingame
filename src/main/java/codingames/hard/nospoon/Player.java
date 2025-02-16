package codingames.hard.nospoon;

import java.util.*;
import java.io.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;

class Node {
    int id, x, y, nodeAmount;
    Set<Node> adj = new HashSet<>();
    Set<Map<Integer, Integer>> config = new HashSet<>();

    public Node(int id, int i, int j, int nodeAmount) {
        this.id = id;
        this.x = i;
        this.y = j;
        this.nodeAmount = nodeAmount;
    }

    private <T, U, R> List<R> cartesian(List<T> strs, List<U> ints, BiFunction<T, U, R> joiner) {
        var out = new ArrayList<R>(strs.size() * ints.size());

        for (T str : strs) {
            for (U i : ints) {
                out.add(joiner.apply(str, i));
            }
        }

        return out;
    }

    private List<int[]> fold(List<int[]> strLst, int[] str) {
        var out = new ArrayList<int[]>(strLst.size() + 1);
        out.addAll(strLst);
        out.add(str);
        return out;
    }

    void findConfig() {
        var tmpConf = new ArrayList<List<int[]>>();

        for (Node node : adj) {
            var edgNum = Math.min(node.nodeAmount, Math.min(nodeAmount, 2));
            var ints = new ArrayList<int[]>();

            for (int i = 0; i <= edgNum; i++) {
                ints.add(new int[] { node.id, i });
            }
            tmpConf.add(ints);
        }

        var it = tmpConf.iterator();
        var cartesianProduct = it.next().stream().map(List::of).toList();

        while (it.hasNext()) {
            var nextList = it.next();
            cartesianProduct = cartesian(cartesianProduct, nextList, this::fold);
        }

        var filteredConfig = cartesianProduct.stream()
                .filter(config -> config.stream().mapToInt(cfg -> cfg[1]).sum() == nodeAmount)
                .toList();
        return;
    }
}

public class Player {
    private static final String filename = "resources/nospoon/cg.txt";
    private static int width, height;
    private static List<Node> nodes = new ArrayList<>();

    private static int[] toMatrix(int number) {
        return new int[] { number % width, number / width };
    }

    private static int toNumber(int x, int y) {
        return y * width + x % width;
    }

    private static void findAdj(Node node, Map<Integer, Node> nodes) {
        var xx = node.x;
        var yy = node.y;

        while (xx > 0) {
            xx -= 1;
            var nodeOpt = nodes.get(toNumber(xx, yy));
            if (nodeOpt != null) {
                node.adj.add(nodeOpt);
                break;
            }
        }

        xx = node.x;
        while (xx < width - 1) {
            xx += 1;
            var nodeOpt = nodes.get(toNumber(xx, yy));
            if (nodeOpt != null) {
                node.adj.add(nodeOpt);
                break;
            }
        }

        xx = node.x;
        while (yy > 0) {
            yy -= 1;
            var nodeOpt = nodes.get(toNumber(xx, yy));
            if (nodeOpt != null) {
                node.adj.add(nodeOpt);
                break;
            }
        }

        yy = node.y;
        while (yy < height - 1) {
            yy += 1;
            var nodeOpt = nodes.get(toNumber(xx, yy));
            if (nodeOpt != null) {
                node.adj.add(nodeOpt);
                break;
            }
        }
    }


    public static void main(String[] args) throws FileNotFoundException {
        var list1 = List.of("No", "One", "Lives");
        var list4 = List.of("Be", "Quick");
        var list7 = List.of("Or", "Dead");

        var metaList = List.of(list1, list4, list7);
        var it = metaList.iterator();
        var list1Lst = it.next().stream().map(List::of).toList();

//        while (it.hasNext()) {
//            var nextList = it.next();
//            list1Lst = cartesian0(list1Lst, nextList, Player::joinStrLst);
//        }

//        if (true) return;
        var readIn = args.length > 0 ? new FileInputStream(filename) : System.in;
        var in = new Scanner(readIn);

        width = in.nextInt();
        height = in.nextInt();

        System.err.println(width);
        System.err.println(height);

        if (in.hasNextLine()) in.nextLine();

        for (int row = 0; row < height; row++) {
            var line = in.nextLine(); // width characters, each either a number or a '.'
            System.err.println(line);
            var res = line.toCharArray();
            for (int col = 0; col < res.length; col++) {
                var sym = res[col];
                if (sym != '.') {
                    int nodeAmount =  sym - '0';
                    var n = new Node(toNumber(col, row), col, row, nodeAmount);
                    nodes.add(n);
                }
            }
        }

        var nodeMap = nodes.stream().collect(Collectors.toMap(node -> node.id, Function.identity()));

        nodes.forEach(node -> findAdj(node, nodeMap));
        nodes.get(9).findConfig();

        System.err.println("Nodes: " + nodes.size());

        System.out.println("0 0 2 0 1");
        System.out.println("2 0 2 2 1");
    }
}
