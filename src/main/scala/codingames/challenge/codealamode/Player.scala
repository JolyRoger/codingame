//package codingames.challenge.codealamode

object Player extends App {
//  case class Recipe (ingr: List[Map[String, Int]], f: List[Map[String, Int]] => List[Map[String, Int]])
  type Point = (Int, Int)
  type Recipe = List[Map[String, Int]]
  var prevState = ""
  var prevPlayeritem = ""
  var myState = prevState
  var target = ""

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
    "NONE-WAITING-FOR-OVEN" -> /*"STRAWBERRIES"*/ "WAIT",
    "STRAWBERRIES-WAITING-FOR-OVEN" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES-WAITING-FOR-OVEN" -> "TABLE-OVEN",
    "CHOPPED_STRAWBERRIES-OVEN-IS-READY" -> "TABLE-OVEN",
    "NONE-OVEN-IS-READY" -> "OVEN",
    "DOUGH" -> "OVEN")
  val croissantChoppedStrawberries= Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE-WAITING-FOR-OVEN" -> "DISH",
    "DISH-WAITING-FOR-OVEN" -> "MOVE-OVEN",
    "DISH-OVEN-IS-READY" -> "OVEN",
    "DISH-CROISSANT" -> "TABLE-STRAWBERRIES",
    "NONE-CROISSANT-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT",
    "NONE-CHOPPED_STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT-CHOPPED_STRAWBERRIES",
    "DISH-CROISSANT-CHOPPED_STRAWBERRIES" -> "WINDOW"
  )
  val croissantBlueberries = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE-WAITING-FOR-OVEN" -> "DISH",
    "DISH-WAITING-FOR-OVEN" -> "MOVE-OVEN",
    "DISH-OVEN-IS-READY" -> "OVEN",
    "DISH-CROISSANT" -> "BLUEBERRIES",
    "DISH-CROISSANT-BLUEBERRIES" -> "WINDOW"
  )
  val croissant = Map("NONE" -> "DOUGH",
    "NONE-WAITING-FOR-OVEN" -> "DISH",
    "DISH-WAITING-FOR-OVEN" -> "MOVE-OVEN",
    "DISH-OVEN-IS-READY" -> "OVEN",
    "DISH-CROISSANT" -> "WINDOW"
  )
  val croissantBlueberriesIcecream = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE-WAITING-FOR-OVEN" -> "DISH",
    "DISH-WAITING-FOR-OVEN" -> "MOVE-OVEN",
    "DISH-OVEN-IS-READY" -> "OVEN",
    "DISH-CROISSANT" -> "BLUEBERRIES",
    "DISH-CROISSANT-BLUEBERRIES" -> "ICE_CREAM",
    "DISH-CROISSANT-BLUEBERRIES-ICE_CREAM" -> "WINDOW"
  )
  val iceCreamCroissant = Map("NONE" -> "DOUGH",
    "NONE-WAITING-FOR-OVEN" -> "DISH",
    "DISH-WAITING-FOR-OVEN" -> "MOVE-OVEN",
    "DISH-OVEN-IS-READY" -> "OVEN",
    "DISH-CROISSANT" -> "ICE_CREAM",
    "DISH-ICE_CREAM-CROISSANT" -> "WINDOW",
  )
  val iceCreamCroissantChoppedStrawberries = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE-WAITING-FOR-OVEN" -> "DISH",
    "DISH-WAITING-FOR-OVEN" -> "MOVE-OVEN",
    "DISH-OVEN-IS-READY" -> "OVEN",
    "DISH-CROISSANT" -> "TABLE-STRAWBERRIES",
    "NONE-CROISSANT-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT",
    "DISH-CROISSANT-CHOPPED_STRAWBERRIES" -> "ICE_CREAM",
    "DISH-CROISSANT-ICE_CREAM-CHOPPED_STRAWBERRIES" -> "WINDOW",
  )

  val iceCreamBlueberriesCroissantChoppedStrawberries = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE-WAITING-FOR-OVEN" -> "DISH",
    "DISH-WAITING-FOR-OVEN" -> "MOVE-OVEN",
    "DISH-OVEN-IS-READY" -> "OVEN",
    "DISH-CROISSANT" -> "TABLE-STRAWBERRIES",
    "STRAWBERRIES-WAITING-FOR-OVEN" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES-WAITING-FOR-OVEN" -> "TABLE-OVEN",
    "CHOPPED_STRAWBERRIES-OVEN-IS-READY" -> "TABLE-OVEN",
    "NONE-OVEN-IS-READY" -> "OVEN")

  val cookBook = Map("DISH-CROISSANT-BLUEBERRIES" -> croissantBlueberries,
                     "DISH-CROISSANT" -> croissant,
                     "DISH-BLUEBERRIES-ICE_CREAM" -> iceCreamBlueberries,
                     "DISH-CROISSANT-BLUEBERRIES-ICE_CREAM" -> croissantBlueberriesIcecream,
                     "DISH-BLUEBERRIES-ICE_CREAM-CROISSANT" -> croissantBlueberriesIcecream,
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
                     "DISH-BLUEBERRIES-CROISSANT-ICE_CREAM" -> croissantBlueberriesIcecream,
                     "DISH-BLUEBERRIES-CROISSANT" -> croissantBlueberries,
                     "DISH-BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT" -> blueberriesCroissantChoppedStrawberries,
                     "DISH-ICE_CREAM-BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT" -> iceCreamBlueberriesCroissantChoppedStrawberries,
                     "DISH-CROISSANT-BLUEBERRIES-ICE_CREAM-CHOPPED_STRAWBERRIES" -> iceCreamBlueberriesCroissantChoppedStrawberries,
                     "DISH-CROISSANT-CHOPPED_STRAWBERRIES" -> croissantChoppedStrawberries,
                     "DISH-CHOPPED_STRAWBERRIES-CROISSANT" -> croissantChoppedStrawberries,
                     "DISH-CHOPPED_STRAWBERRIES-CROISSANT-ICE_CREAM" -> iceCreamCroissantChoppedStrawberries,
                     "DISH-CROISSANT-ICE_CREAM-CHOPPED_STRAWBERRIES" -> iceCreamCroissantChoppedStrawberries,
                     "DISH-CROISSANT-ICE_CREAM" -> iceCreamCroissant,
                     "DISH-CHOPPED_STRAWBERRIES-CROISSANT-BLUEBERRIES-ICE_CREAM" -> iceCreamBlueberriesCroissantChoppedStrawberries,
                     "DISH-CROISSANT-ICE_CREAM-CHOPPED_STRAWBERRIES-BLUEBERRIES" -> iceCreamBlueberriesCroissantChoppedStrawberries,
                     "DISH-CHOPPED_STRAWBERRIES-BLUEBERRIES-CROISSANT-ICE_CREAM" -> iceCreamBlueberriesCroissantChoppedStrawberries
  )


  implicit def toNumber(point: Point): Int = point._2 * 11 + point._1 % 11
  def toString(point: Point): String = if (point._1 < 0) "" else s"${point._1} ${point._2}"
  def toMatrix(number: Int): Point = (number % 11, number / 11)
  def walk(matrix: List[String], row: Int, col: Int) = row > 0 && row < 7 && col > 0 && col < 11 && (matrix(row)(col) == '.' || matrix(row)(col) == '0')
  def isEmptyTable(matrix: List[String], row: Int, col: Int) = row >= 0 && row < 7 && col >= 0 && col < 11 && (matrix(row)(col) == '#')
  def replaceSym(matrix: List[String], oldSym: Char, newSym: Char) = matrix.map(_.replace(oldSym, newSym))
  def searchClosestEmptyTable(matrix: List[String], point: Point) = {
    Console.err.println(s"CLOSEST EMPTY TABLE: $point")
    List((point._1 - 1, point._2 - 1), (point._1, point._2 - 1),
      (point._1 + 1, point._2 - 1), (point._1 - 1, point._2), (point._1 + 1, point._2), (point._1 - 1, point._2 + 1),
      (point._1, point._2 + 1), (point._1 + 1, point._2 + 1)).filter(p => isEmptyTable(matrix, p._2, p._1)).head
  }

  def reach(graphMatrix: List[String], target: String, from: Point, dynamicMap: Map[String, List[(Int, Int)]]) = {
    Console.err.println(s"TARGETPOINT::target: $target")
    if (target == "TABLE") ("USE", searchClosestEmptyTable(graphMatrix, from))
    else if (target.startsWith("MOVE")) ("MOVE", dynamicMap.getOrElse(target.split('-')(1), List(searchClosestEmptyTable(graphMatrix, from))).head)
    else if (target.startsWith("TABLE")) ("USE", searchClosestEmptyTable(graphMatrix, dynamicMap.getOrElse(target.split('-')(1), List(searchClosestEmptyTable(graphMatrix, from))).head))
    else if (target == "WAIT") ("WAIT", (-1,-1)) else ("USE", dynamicMap.getOrElse(target, List(searchClosestEmptyTable(graphMatrix, from))).head)
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

  val targetStaticMap = Map("DISH" -> List(searchSym(graphMatrix, 'D').getOrElse((-1,-1))),
                      "ICE_CREAM" -> List(searchSym(graphMatrix, 'I').getOrElse((-1,-1))),
                      "BLUEBERRIES" -> List(searchSym(graphMatrix, 'B').getOrElse((-1,-1))),
                      "STRAWBERRIES" -> List(searchSym(graphMatrix, 'S').getOrElse((-1,-1))),
                      "CHOPPING" -> List(searchSym(graphMatrix, 'C').getOrElse((-1,-1))),
                      "OVEN" -> List(searchSym(graphMatrix, 'O').getOrElse((-1,-1))),
                      "DOUGH" -> List(searchSym(graphMatrix, 'H').getOrElse((-1,-1))),
                      "WINDOW" -> List(searchSym(graphMatrix, 'W').getOrElse((-1,-1))))

  def addToMap(oldMap: Map[String, List[(Int, Int)]], key: String, value: (Int, Int)) =
    oldMap + (key -> (oldMap.get(key) match {
      case Some(lst) => value :: lst
      case None => List(value)
    }))
  val stateBook = Map(("OVEN", "DOUGH") -> "-WAITING-FOR-OVEN",
                      ("STRAWBERRIES", "NONE-WAITING-FOR-OVEN") -> "-WAITING-FOR-OVEN",
                      ("DISH", "NONE-WAITING-FOR-OVEN") -> "-WAITING-FOR-OVEN",
                      ("CHOPPING", "STRAWBERRIES-WAITING-FOR-OVEN") -> "-WAITING-FOR-OVEN",
                      ("TABLE-STRAWBERRIES", "DISH-CROISSANT") -> "-CROISSANT-ON-A-TABLE",
                      ("STRAWBERRIES", "NONE-CROISSANT-ON-A-TABLE") -> "-CROISSANT-ON-A-TABLE",
                      ("DISH-CROISSANT", "CHOPPED_STRAWBERRIES-CROISSANT-ON-A-TABLE") -> "-CHOPPED_STRAWBERRIES-CROISSANT-ON-A-TABLE",
                      ("CHOPPING", "STRAWBERRIES-CROISSANT-ON-A-TABLE") -> "-CROISSANT-ON-A-TABLE"
  )

  def newState(prevPlayeritem: String, playeritem: String, prevTarget: String, myState: String) = {
//    stateBook.foreach(aa => Console.err.println(s"stateBook: $aa"))
    Console.err.println(s"\tprevPlayeritem: $prevPlayeritem newPlayeitem: $playeritem\n\tprevState: $prevState prevTarget: $prevTarget stateCandidate: $myState" +
      s"\tstateBook contains: ${stateBook.contains((prevTarget, myState))}")

    playeritem + stateBook.getOrElse((prevTarget, myState), "")
  }

  // game loop
  while (true) {
    var dynamicMap = targetStaticMap
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
      dynamicMap = addToMap(dynamicMap, item, (tablex, tabley))
    }
    // ovencontents: ignore until wood 1 league
    val Array(ovencontents, _oventimer) = readLine split " "
    dynamicMap = addToMap(dynamicMap, ovencontents, searchSym(graphMatrix, 'O').get)
    dynamicMap.foreach(d => Console.err.println(s"Dynamic map content: ${d._1} -> ${d._2}"))
    if (ovencontents == "CROISSANT") myState = myState.replace("WAITING-FOR-OVEN", "OVEN-IS-READY")
    val oventimer = _oventimer.toInt
   Console.err.println(s"ovencontents: $ovencontents, oventimer: $oventimer")
    val numcustomers = readInt // the number of customers currently waiting for food
    // Console.err.println(s"numcustomers: $numcustomers")
    val customers = (for (i <- 0 until numcustomers) yield readLine split " ").toList.sortBy(_(1))
    customers.foreach(c => Console.err.println(s"customeritem: ${c(0)}, customeraward: ${c(1)}"))
    val trg = nextTarget(myState, customers/*.map(_(0))*/)
    Console.err.println(s"trg: $trg")
    val action = reach(graphMatrix, trg, (playerx, playery), dynamicMap)
    if (action._2._1 >= 0) target = /*action._2*/ trg

    println(s"${action._1} ${toString(action._2)}")
  }
}