input = new Scanner(System.in)

input.nextInt()
H = input.nextInt()
input.nextLine()

def matrix = new String[H]

for (i = 0; i < H; ++i) matrix[i] = input.nextLine()

input.nextInt() // the coordinate along the X axis of the exit (not useful for this first mission, but must be read).

while (true) {
    XI = input.nextInt()
    YI = input.nextInt()

	def (newX, newY) =  [	 { String pos, int x, int y -> [x, y]   },
							 { String pos, int x, int y -> [x, y+1] },
							 { String pos, int x, int y -> if (pos == "LEFT") return [x+1, y] else return [x-1, y] },
							 { String pos, int x, int y -> [x, y+1] },
							 { String pos, int x, int y -> if (pos == "RIGHT") return [x, y+1] else return [x-1, y] },
							 { String pos, int x, int y -> if (pos == "LEFT") return [x, y+1] else return [x+1, y] },
							 { String pos, int x, int y -> if (pos == "LEFT") return [x+1, y] else return [x-1, y] },
							 { String pos, int x, int y -> [x, y+1] },
							 { String pos, int x, int y -> [x, y+1] },
							 { String pos, int x, int y -> [x, y+1] },
							 { String pos, int x, int y -> [x-1, y] },
							 { String pos, int x, int y -> [x+1, y] },
							 { String pos, int x, int y -> [x, y+1] },
							 { String pos, int x, int y -> [x, y+1] }
	] [ matrix[YI].split(' ')[XI].toInteger() ] (input.next(), XI, YI)

    println "${newX} ${newY}"
}