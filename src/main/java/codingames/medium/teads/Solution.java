package codingames.medium.teads;

import java.util.*;

class Graph {
	private HashMap<Integer, ArrayList<Integer>> adj;
	private int e;
	private HashMap<Integer, Integer> resmap = new HashMap<>();

	Graph() {
		Scanner in = new Scanner(System.in);
		e = in.nextInt();

		adj = new HashMap<>();

		for (int i = 0; i < e; i++) {
			int xi = in.nextInt();
			int yi = in.nextInt();
			addEdge(xi, yi);
		}
	}

	private void addEdge(int v, int w) {
		addSingleEdge(v, w);
		addSingleEdge(w, v);
	}

	private void addSingleEdge(int v1, int v2) {
		ArrayList<Integer> l = adj.get(v1);
		if (l == null) l = new ArrayList<>();
		l.add(v2); adj.put(v1, l);
	}

	private int maxTime(int s, int min) {
		if (resmap.get(s) != null && resmap.get(s) > min) return min;

		ArrayDeque<Integer> q = new ArrayDeque<>();
		int vertexAmount = adj.size();
		HashMap<Integer, Integer> distTo = new HashMap<>(vertexAmount);
		HashMap<Integer, Boolean> marked = new HashMap<>(vertexAmount);
		q.offer(s);
		marked.put(s, Boolean.TRUE);
		distTo.put(s, 0);

		while (!q.isEmpty()) {
			int v = q.poll();
			List<Integer> vAdj = adj.get(v);
			for (Integer w : vAdj) {
				if (marked.get(w) == null) {
						q.offer(w);
						marked.put(w, Boolean.TRUE);
						int newRes = distTo.get(v) + 1;
						distTo.put(w, newRes);
						Integer ss = resmap.get(s);
						if (ss == null || ss < newRes) resmap.put(s, newRes);
						Integer sw = resmap.get(w);
						if (sw == null || sw < newRes) resmap.put(w, newRes);
				}
			}
		}
		return Collections.max(distTo.values());
	}

	int teads() {
		int min = Integer.MAX_VALUE;
		for (Integer v : adj.keySet()) {
			int vMax = maxTime(v, min);
			if (vMax < min) min = vMax;
		}
		return min;
	}
}

public class Solution {
	public static void main(String args[]) {
		System.out.println(new Graph().teads());
	}
}