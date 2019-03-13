//package codingames.challenge.codealamode

object Player extends App {
//  case class Recipe (ingr: List[Map[String, Int]], f: List[Map[String, Int]] => List[Map[String, Int]])
  type Point = (Int, Int)
  type Recipe = List[Map[String, Int]]
  var prevState = ""
  var prevPlayeritem = ""
  var myState = prevState
  var target = (-1, -1)

  val iceCreamBlueberries = Map("NONE" -> "DISH", "DISH" -> "BLUEBERRIES", "DISH-BLUEBERRIES" -> "ICE_CREAM", "DISH-BLUEBERRIES-ICE_CREAM" -> "WINDOW", "DISH-ICE_CREAM-BLUEBERRIES" -> "WINDOW")
  val iceCreamChoppedStrawberriesBlueberries = Map("NONE" -> "STRAWBERRIES", "STRAWBERRIES" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES" -> "DISH", "DISH-CHOPPED_STRAWBERRIES" -> "ICE_CREAM", "DISH-CHOPPED_STRAWBERRIES-ICE_CREAM" -> "BLUEBERRIES",
    "DISH-ICE_CREAM-CHOPPED_STRAWBERRIES" -> "BLUEBERRIES", "DISH-ICE_CREAM-CHOPPED_STRAWBERRIES-BLUEBERRIES" -> "WINDOW", "DISH-CHOPPED_STRAWBERRIES-ICE_CREAM-BLUEBERRIES" -> "WINDOW")
  val сhoppedStrawberriesBlueberries = Map("NONE" -> "STRAWBERRIES", "STRAWBERRIES" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES" -> "DISH", "DISH-CHOPPED_STRAWBERRIES" -> "BLUEBERRIES", "DISH-CHOPPED_STRAWBERRIES-BLUEBERRIES" -> "WINDOW")
  val iceCreamChoppedStrawberries = Map("NONE" -> "STRAWBERRIES", "STRAWBERRIES" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES" -> "DISH", "DISH-CHOPPED_STRAWBERRIES" -> "ICE_CREAM",
    "DISH-ICE_CREAM-CHOPPED_STRAWBERRIES" -> "WINDOW", "DISH-CHOPPED_STRAWBERRIES-ICE_CREAM" -> "WINDOW")
  val blueberriesCroissantChoppedStrawberries = Map("NONE" -> "DOUGH",
    "NONE-WAITING-FOR-CROISSANT" -> /*"STRAWBERRIES"*/ "WAIT",
    "STRAWBERRIES-WAITING-FOR-CROISSANT" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES-WAITING-FOR-CROISSANT" -> "TABLE-OVEN",
    "CHOPPED_STRAWBERRIES-CROISSANT-IS-READY" -> "TABLE-OVEN",
    "NONE-CROISSANT-IS-READY" -> "OVEN",
    "DOUGH" -> "OVEN")
  val iceCreamBlueberriesCroissantChoppedStrawberries = Map("NONE" -> "DOUGH",
    "NONE-WAITING-FOR-CROISSANT" -> /*"STRAWBERRIES"*/ /*"WAIT*/ "DISH",
    "DISH-WAITING-FOR-CROISSANT" -> "MOVE-OVEN",
    "DISH-CROISSANT-IS-READY" -> "OVEN",
    "DISH-CROISSANT" -> "TABLE-STRAWBERRIES",
    "STRAWBERRIES-WAITING-FOR-CROISSANT" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES-WAITING-FOR-CROISSANT" -> "TABLE-OVEN",
    "CHOPPED_STRAWBERRIES-CROISSANT-IS-READY" -> "TABLE-OVEN",
    "NONE-CROISSANT-IS-READY" -> "OVEN",
    "DOUGH" -> "OVEN")

  val cookBook = Map("DISH-ICE_CREAM-BLUEBERRIES" -> iceCreamBlueberries,
                     "DISH-BLUEBERRIES-ICE_CREAM" -> iceCreamBlueberries,
                     "DISH-CHOPPED_STRAWBERRIES-ICE_CREAM-BLUEBERRIES" -> iceCreamChoppedStrawberriesBlueberries,
                     "DISH-CHOPPED_STRAWBERRIES-BLUEBERRIES-ICE_CREAM" -> iceCreamChoppedStrawberriesBlueberries,
                     "DISH-ICE_CREAM-CHOPPED_STRAWBERRIES-BLUEBERRIES" -> iceCreamChoppedStrawberriesBlueberries,
                     "DISH-BLUEBERRIES-CHOPPED_STRAWBERRIES-ICE_CREAM" -> iceCreamChoppedStrawberriesBlueberries,
                     "DISH-ICE_CREAM-BLUEBERRIES-CHOPPED_STRAWBERRIES" -> iceCreamChoppedStrawberriesBlueberries,
                     "DISH-BLUEBERRIES-ICE_CREAM-CHOPPED_STRAWBERRIES" -> iceCreamChoppedStrawberriesBlueberries,
                     "DISH-CHOPPED_STRAWBERRIES-BLUEBERRIES" -> сhoppedStrawberriesBlueberries,
                     "DISH-BLUEBERRIES-CHOPPED_STRAWBERRIES" -> сhoppedStrawberriesBlueberries,
                     "DISH-ICE_CREAM-CHOPPED_STRAWBERRIES" -> iceCreamChoppedStrawberries,
                     "DISH-CHOPPED_STRAWBERRIES-ICE_CREAM" -> iceCreamChoppedStrawberries,
                     "DISH-BLUEBERRIES-CROISSANT-CHOPPED_STRAWBERRIES" -> blueberriesCroissantChoppedStrawberries,
                     "DISH-ICE_CREAM-BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT" -> iceCreamBlueberriesCroissantChoppedStrawberries
  )


  implicit def toNumber(point: Point): Int = point._2 * 11 + point._1 % 11
  def toString(point: Point): String = if (point._1 < 0) "" else s"${point._1} ${point._2}"
  def toMatrix(number: Int): Point = (number % 11, number / 11)
  def walk(matrix: List[String], row: Int, col: Int) = row > 0 && row < 7 && col > 0 && col < 11 && (matrix(row)(col) == '.' || matrix(row)(col) == '0')
  def isEmptyTable(matrix: List[String], row: Int, col: Int) = row >= 0 && row <= 7 && col >= 0 && col <= 11 && (matrix(row)(col) == '#')
  def replaceSym(matrix: List[String], oldSym: Char, newSym: Char) = matrix.map(_.replace(oldSym, newSym))
  def searchClosestEmptyTable(matrix: List[String], point: Point) = {
    Console.err.println(s"CLOSEST EMPTY TABLE")
    List((point._1 - 1, point._2 - 1), (point._1, point._2 - 1),
      (point._1 + 1, point._2 - 1), (point._1 - 1, point._2), (point._1 + 1, point._2), (point._1 - 1, point._2 + 1),
      (point._1, point._2 + 1), (point._1 + 1, point._2 + 1)).filter(p => isEmptyTable(matrix, p._2, p._1)).head
  }

  def reach(target: String, from: Point) = {
    Console.err.println(s"TARGETPOINT::target: $target")
    if (target == "TABLE") ("USE", searchClosestEmptyTable(graphMatrix, from))
    else if (target.startsWith("MOVE")) ("MOVE", targetMap(target.split('-')(1)))
    else if (target.startsWith("TABLE")) ("USE", searchClosestEmptyTable(graphMatrix, targetMap(target.split('-')(1))))
    else if (target == "WAIT") ("WAIT", (-1,-1)) else ("USE" ,targetMap(target))
  }

  def searchSym(matrix: List[String], sym: Char) = {
    val coords = for (row <- matrix.indices; col <- matrix(row).indices; _sym = matrix(row)(col); if _sym == sym) yield (col, row)
    if (coords.nonEmpty) Some(coords(0)) else None
  }
  def adjTo(matrix: List[String], point: Point): List[Point] = List((point._1 - 1, point._2 - 1), (point._1, point._2 - 1),
                   (point._1 + 1, point._2 - 1), (point._1 - 1, point._2), (point._1 + 1, point._2), (point._1 - 1, point._2 + 1),
                   (point._1, point._2 + 1), (point._1 + 1, point._2 + 1)).filter(p => walk(matrix, p._2, p._1))

  def nextTarget(myState: String, customerItems: List[Array[String]]) = {
//    customerItems.foreach(item => Console.err.println(s"customerItem: ${item(0)} - ${item(1)}\thave: $myState"))
    val properOrderList = customerItems.filter(orderData => cookBook(orderData(0)).contains(myState))
    val properOrder = properOrderList.headOption.getOrElse(Array("NO SUCH ORDER! START A NEW ONE!"))(0)
    Console.err.println(s"want to cook: $properOrder\thave: $myState")
    val recipe = cookBook.getOrElse(properOrder, Map.empty[String, String])
    Console.err.println(s"recipe is empty: ${recipe.isEmpty}\tmyState: $myState")
    recipe.getOrElse(myState, "TABLE")
  }

  val numallcustomers = readInt
  // Console.err.println(s"numallcustomers: $numallcustomers")

  val customerData = (for (i <- 0 until numallcustomers) yield readLine split " ").toList

  val graphMatrix = (for (i <- 0 until 7) yield readLine).toList

  val targetMap = Map("DISH" -> searchSym(graphMatrix, 'D').getOrElse((-1,-1)),
                      "ICE_CREAM" -> searchSym(graphMatrix, 'I').getOrElse((-1,-1)),
                      "BLUEBERRIES" -> searchSym(graphMatrix, 'B').getOrElse((-1,-1)),
                      "STRAWBERRIES" -> searchSym(graphMatrix, 'S').getOrElse((-1,-1)),
                      "CHOPPING" -> searchSym(graphMatrix, 'C').getOrElse((-1,-1)),
                      "OVEN" -> searchSym(graphMatrix, 'O').getOrElse((-1,-1)),
                      "DOUGH" -> searchSym(graphMatrix, 'H').getOrElse((-1,-1)),
                      "WINDOW" -> searchSym(graphMatrix, 'W').getOrElse((-1,-1)))
//                      "EMPTY" -> searchClosestEmptyTable((graphMatrix, 'W').getOrElse((-1,-1)))

  val stateBook = Map((targetMap("OVEN"), "DOUGH") -> "-WAITING-FOR-CROISSANT",
                      (targetMap("STRAWBERRIES"), "NONE-WAITING-FOR-CROISSANT") -> "-WAITING-FOR-CROISSANT",
                      (targetMap("DISH"), "NONE-WAITING-FOR-CROISSANT") -> "-WAITING-FOR-CROISSANT",
                      (targetMap("CHOPPING"), "STRAWBERRIES-WAITING-FOR-CROISSANT") -> "-WAITING-FOR-CROISSANT")

  def newState(prevPlayeritem: String, playeritem: String, prevTarget: Point, myState: String) = {
//    stateBook.foreach(aa => Console.err.println(s"stateBook: $aa"))
    Console.err.println(s"newState::prevState: $prevState\t prevTarget: $prevTarget\tstateCandidate: $myState" +
      s"\tstateBook contains: ${stateBook.contains((prevTarget, myState))}")

    playeritem + stateBook.getOrElse((prevTarget, myState), "")
  }

  // game loop
  while (true) {
    val turnsremaining = readInt
    // Console.err.println(s"turnsremaining: $turnsremaining")

    val Array(_playerx, _playery, playeritem) = readLine split " "
    if (playeritem != prevPlayeritem) {
      myState = newState(prevPlayeritem, playeritem, target, myState)
      prevPlayeritem = playeritem
    }

    val playerx = _playerx.toInt
    val playery = _playery.toInt
    // Console.err.println(s"playerx: $playerx, playery: $playery, playeritem: $playeritem")
    val Array(_partnerx, _partnery, partneritem) = readLine split " "
    val partnerx = _partnerx.toInt
    val partnery = _partnery.toInt
//   Console.err.println(s"partnerx: $partnerx, partnery: $partnery, partneritem: $partneritem")
    val numtableswithitems = readInt // the number of tables in the kitchen that currently hold an item
     Console.err.println(s"numtableswithitems: $numtableswithitems")
    for (i <- 0 until numtableswithitems) {
      val Array(_tablex, _tabley, item) = readLine split " "
      val tablex = _tablex.toInt
      val tabley = _tabley.toInt
       Console.err.println(s"\t($tablex $tabley) - $item")
    }
    // ovencontents: ignore until wood 1 league
    val Array(ovencontents, _oventimer) = readLine split " "
    if (ovencontents == "CROISSANT") myState = myState.replace("WAITING-FOR-CROISSANT", "CROISSANT-IS-READY")
    val oventimer = _oventimer.toInt
   Console.err.println(s"ovencontents: $ovencontents, oventimer: $oventimer")
    val numcustomers = readInt // the number of customers currently waiting for food
    // Console.err.println(s"numcustomers: $numcustomers")
    val customers = (for (i <- 0 until numcustomers) yield readLine split " ").toList.sortBy(_(1))
//    customers.foreach(c => Console.err.println(s"customeritem: ${c(0)}, customeraward: ${c(1)}"))
    val trg = nextTarget(myState, customers/*.map(_(0))*/)
    val action = reach(trg, (playerx, playery))
    if (action._2._1 > 0) target = action._2

    println(s"${action._1} ${toString(action._2)}")
  }
}