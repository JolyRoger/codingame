package codingames.hard.nospoon;

import java.util.*;
import java.io.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;

class Player {
    record Link(Node from, Node to, int num) {

        @Override
        public int hashCode() {
            return from.hashCode() + to.hashCode() + num;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) return true;
            if (obj == null) return false;
            if (getClass() != obj.getClass()) return false;

            var lnk = (Link) obj;

            return (from.id == lnk.from().id && to.id == lnk.to().id) ||
                    (from.id == lnk.to().id && to.id == lnk.from().id);
        }

        @Override
        public String toString() {
            return from.x + " " + from.y + " " + to.x + " " + to.y + " " + num;
        }
    }

    static class Node {
        int id, x, y, nodeAmount;
        Set<Node> adj = new HashSet<>();
        Set<List<Link>> config = new HashSet<>();

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

        void findConfig(Map<Integer, Node> nodeMap) {
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

            config = cartesianProduct.stream()
                    .filter(localConfig -> localConfig.stream().mapToInt(cfg -> cfg[1]).sum() == nodeAmount)
                    .map(mappedConfig -> mappedConfig.stream().map(mcfg -> new Link(this, nodeMap.get(mcfg[0]), mcfg[1])).toList())
                    .collect(Collectors.toSet());
        }
    }

    private static int width, height;
    private static final List<Node> nodes = new ArrayList<>();
    private static Map<Integer, Node> nodeMap = new HashMap<>();
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

    private static void applyConfig(Node node, List<Link> config) {
        config.forEach(link -> {
            var toNode = nodeMap.get(link.to().id);
            toNode.config = toNode.config.stream()
                    .filter(links -> links.stream().anyMatch(l -> l.to().id == node.id && l.num() == link.num()))
                    .collect(Collectors.toSet());
        });
    }

    public static void main(String[] args) throws FileNotFoundException {
        var readIn = args.length > 0 ? new FileInputStream("resources/nospoon/" + args[0] + ".txt") : System.in;
        var in = new Scanner(readIn);

        var strategyStack = new Stack<List<Link>>();

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


        nodeMap = nodes.stream().collect(Collectors.toMap(node -> node.id, Function.identity()));

        nodes.forEach(node -> {
            findAdj(node, nodeMap);
            node.findConfig(nodeMap);
        });

//        System.err.println("Nodes: " + nodes.size());

//        nodes.forEach(node -> {
//            System.err.println("id=" + node.id + " configs=" + node.config.size());
//            node.config.forEach(cfg -> {
//                System.err.println("----");
//                cfg.forEach(link -> System.err.println("\t" + link.to().id + " -> " + link.num()));
//            });
//        });

        var oneStrategyNodeList = nodes.stream().filter(n -> n.config.size() == 1).toList();

        while (!oneStrategyNodeList.isEmpty()) {
            oneStrategyNodeList.forEach(node -> {
                var config = node.config.stream().findFirst().get();
                applyConfig(node, config);
                node.config = Set.of();
                strategyStack.push(config);
            });
            oneStrategyNodeList = nodes.stream().filter(n -> n.config.size() == 1).toList();
        }

        var setStack = strategyStack.stream()
                .flatMap(strategy -> strategy.stream().filter(link -> link.num() != 0))
                .collect(Collectors.toSet());

        setStack.forEach(System.out::println);
    }
}
