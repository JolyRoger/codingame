package codingames;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Sample {
	public static void main(String[] args) {
		String s = "....";
		String word = "...";
		String reg = word.replace(".", "\\.");
		int wordSize = word.length();
		System.out.println(word.length() + "\t\t" + reg.length());

		Matcher m = Pattern.compile("(?=(" + reg + "))").matcher(s);
//		Matcher m = Pattern.compile("\\.\\.\\.").matcher(s);
		while (m.find()) {
			int start = m.start();
			System.out.print(start + "->" + m.end());
			System.out.println("\t" + start + "->" + (start + wordSize));
		}
	}
}
