//package codingames.challenge.codealamode

object Player extends App {
//  case class Recipe (ingr: List[Map[String, Int]], f: List[Map[String, Int]] => List[Map[String, Int]])
  type Point = (Int, Int)
  type Recipe = List[Map[String, Int]]
  var prevState = ""
  var prevPlayeritem = ""
  var myState = prevState
  var target = ""

  val croissant = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE-WAITING-FOR-CROISSANT" -> "DISH",
    "DISH-WAITING-FOR-CROISSANT" -> "MOVE-OVEN",
    "DISH-CROISSANT-IS-READY" -> "OVEN",
    "NONE-CROISSANT-IS-READY" -> "DISH",
    "CROISSANT-DISH" -> "WINDOW"
  )
  val choppedStrawberries = Map("NONE" -> "STRAWBERRIES",
    "STRAWBERRIES" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES" -> "DISH"
  )
  val iceCreamBlueberries = Map("NONE" -> "DISH", "DISH" -> "BLUEBERRIES", "BLUEBERRIES-DISH" -> "ICE_CREAM", "BLUEBERRIES-DISH-ICE_CREAM" -> "WINDOW")
  val iceCreamChoppedStrawberriesBlueberries = Map("NONE" -> "STRAWBERRIES",
    "STRAWBERRIES" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES" -> "DISH",
    "CHOPPED_STRAWBERRIES-DISH" -> "ICE_CREAM",
    "CHOPPED_STRAWBERRIES-DISH-ICE_CREAM" -> "BLUEBERRIES",
    "CHOPPED_STRAWBERRIES-DISH-ICE_CREAM" -> "BLUEBERRIES",
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-DISH-ICE_CREAM" -> "WINDOW",
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-DISH-ICE_CREAM" -> "WINDOW")
  val сhoppedStrawberriesBlueberries = Map("NONE" -> "STRAWBERRIES",
    "STRAWBERRIES" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES" -> "DISH",
    "CHOPPED_STRAWBERRIES-DISH" -> "BLUEBERRIES",
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-DISH" -> "WINDOW")
  val iceCreamChoppedStrawberries = Map("NONE" -> "STRAWBERRIES",
    "STRAWBERRIES" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES" -> "DISH",
    "CHOPPED_STRAWBERRIES-DISH" -> "ICE_CREAM",
    "CHOPPED_STRAWBERRIES-DISH-ICE_CREAM" -> "WINDOW")
  val blueberriesCroissantChoppedStrawberries = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE-WAITING-FOR-CROISSANT" -> "DISH",
    "DISH-WAITING-FOR-CROISSANT" -> "MOVE-OVEN",
    "DISH-CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-DISH" -> "TABLE-STRAWBERRIES",
    "NONE-CROISSANT-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT",
    "CHOPPED_STRAWBERRIES-NONE-CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT-CHOPPED_STRAWBERRIES",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> "BLUEBERRIES",
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> "WINDOW"
    )
  val croissantChoppedStrawberries= Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE-WAITING-FOR-CROISSANT" -> "DISH",
    "DISH-WAITING-FOR-CROISSANT" -> "MOVE-OVEN",
    "DISH-CROISSANT-IS-READY" -> "OVEN",
    "DISH-CROISSANT" -> "TABLE-STRAWBERRIES",
    "NONE-CROISSANT-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT",
    "NONE-CHOPPED_STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT-CHOPPED_STRAWBERRIES",
    "DISH-CROISSANT-CHOPPED_STRAWBERRIES" -> "WINDOW"
  )
  val croissantBlueberries = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE-WAITING-FOR-CROISSANT" -> "DISH",
    "DISH-WAITING-FOR-CROISSANT" -> "MOVE-OVEN",
    "DISH-CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-DISH" -> "BLUEBERRIES",
    "BLUEBERRIES-CROISSANT-DISH" -> "WINDOW"
  )
  val croissantBlueberriesIcecream = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE-WAITING-FOR-CROISSANT" -> "DISH",
    "DISH-WAITING-FOR-CROISSANT" -> "MOVE-OVEN",
    "DISH-CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-DISH" -> "BLUEBERRIES",
    "BLUEBERRIES-CROISSANT-DISH-" -> "ICE_CREAM",
    "BLUEBERRIES-CROISSANT-DISH-ICE_CREAM" -> "WINDOW"
  )
  val iceCreamCroissant = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE-CROISSANT-IS-READY" -> "DISH",
    "NONE-WAITING-FOR-CROISSANT" -> "DISH",
    "DISH-WAITING-FOR-CROISSANT" -> "MOVE-OVEN",
    "DISH-CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-DISH" -> "ICE_CREAM",
    "CROISSANT-DISH-ICE_CREAM" -> "WINDOW",
  )
  val iceCreamCroissantChoppedStrawberries = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE-CROISSANT-IS-READY" -> "DISH",
    "NONE-WAITING-FOR-CROISSANT" -> "DISH",
    "DISH-WAITING-FOR-CROISSANT" -> "MOVE-OVEN",
    "DISH-CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-DISH" -> "TABLE-STRAWBERRIES",
    "NONE-CROISSANT-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> "ICE_CREAM",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH-ICE_CREAM" -> "WINDOW",
  )
  val iceCreamBlueberriesCroissantChoppedStrawberries = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE-CROISSANT-IS-READY" -> "DISH",
    "NONE-WAITING-FOR-CROISSANT" -> "DISH",
    "DISH-WAITING-FOR-CROISSANT" -> "MOVE-OVEN",
    "DISH-CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-DISH" -> "TABLE-STRAWBERRIES",
    "NONE-CROISSANT-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES-CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> "ICE_CREAM",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH-ICE_CREAM-" -> "BLUEBERRIES",
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH-ICE_CREAM" -> "WINDOW",
    "STRAWBERRIES-WAITING-FOR-CROISSANT" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES-WAITING-FOR-CROISSANT" -> "TABLE-OVEN",
    "CHOPPED_STRAWBERRIES-CROISSANT-IS-READY" -> "TABLE-OVEN",
    "NONE-CROISSANT-IS-READY" -> "OVEN")

  val blueberriesChoppedStrawberriesCroissantIceCreamTart = Map("NONE" -> "DOUGH")
  val blueberriesChoppedStrawberriesIceCreamTart = Map("NONE" -> "DOUGH")
  val blueberriesChoppedStrawberriesCroissantTart = Map("NONE" -> "DOUGH")
  val blueberriesCroissantIceCreamTart = Map("NONE" -> "DOUGH")
  val choppedStrawberriesCroissantIceCreamTart = Map(
    "NONE" -> "DOUGH",
    "DOUGH" -> "CHOPPING",
    "CHOPPED_DOUGH" -> "BLUEBERRIES",
    "RAW_TART" -> "OVEN",
    "NONE-WAITING-FOR-TART" -> "DISH",
    "NONE-TART-IS-READY" -> "DISH",
    "DISH-WAITING-FOR-TART" -> "MOVE-OVEN",
    "DISH-TART-IS-READY" -> "OVEN",
    "DISH-TART" -> "TABLE-DOUGH",
    "NONE-TART-ON-A-TABLE" -> "DOUGH",
    "DOUGH-TART-ON-A-TABLE" -> "OVEN",
    "NONE-TART-ON-A-TABLE-WAITING-FOR-CROISSANT" -> "WAIT",
    "NONE-TART-ON-A-TABLE-CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-TART-ON-A-TABLE" -> "DISH-TART",
    "NONE-TART-CROISSANT-ON-A-TABLE" -> "DISH-TART-CROISSANT",
    "CROISSANT-DISH-TART" -> "TABLE-STRAWBERRIES"
  )
//  ("DISH-TART", "CROISSANT-TART-ON-A-TABLE") -> "-TART-CROISSANT-ON-A-TABLE"
  val blueberriesChoppedStrawberriesTart = Map("NONE" -> "DOUGH")
  val blueberriesCroissantTart = Map("NONE" -> "DOUGH")
  val choppedStrawberriesCroissantTart = Map("NONE" -> "DOUGH")
  val blueberriesIceCreamTart = Map("NONE" -> "DOUGH")
  val choppedStrawberriesIceCreamTart = Map("NONE" -> "DOUGH")
  val croissantIceCreamTart = Map("NONE" -> "DOUGH")
  val blueberriesTart = Map("NONE" -> "DOUGH")
  val croissantTart = Map("NONE" -> "DOUGH",
    "DOUGH" -> "CHOPPING",
    "CHOPPED_DOUGH" -> "BLUEBERRIES",
    "RAW_TART" -> "OVEN",
    "NONE-WAITING-FOR-TART" -> "DISH",
    "NONE-TART-IS-READY" -> "DISH",
    "DISH-WAITING-FOR-TART" -> "MOVE-OVEN",
    "DISH-TART-IS-READY" -> "OVEN",
    "DISH-TART" -> "TABLE-DOUGH",
    "NONE-TART-ON-A-TABLE" -> "DOUGH",
    "DOUGH-TART-ON-A-TABLE" -> "OVEN",
    "CROISSANT-TART-ON-A-TABLE" -> "TART"
  )
  val choppedStrawberriesTart = Map("NONE" -> "DOUGH")

  val cookBook = Map("BLUEBERRIES-CROISSANT-DISH" -> croissantBlueberries,
                     "CROISSANT-DISH" -> croissant,
                     "BLUEBERRIES-DISH-ICE_CREAM" -> iceCreamBlueberries,
                     "CHOPPED_STRAWBERRIES-DISH-ICE_CREAM" -> iceCreamChoppedStrawberries,
                     "BLUEBERRIES-CHOPPED_STRAWBERRIES-DISH" -> сhoppedStrawberriesBlueberries,
                     "BLUEBERRIES-CROISSANT-DISH-ICE_CREAM" -> croissantBlueberriesIcecream,
                     "BLUEBERRIES-CHOPPED_STRAWBERRIES-DISH-ICE_CREAM" -> iceCreamChoppedStrawberriesBlueberries,
                     "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> blueberriesCroissantChoppedStrawberries,
                     "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH-ICE_CREAM" -> iceCreamBlueberriesCroissantChoppedStrawberries,
                     "CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> croissantChoppedStrawberries,
                     "CHOPPED_STRAWBERRIES-CROISSANT-DISH-ICE_CREAM" -> iceCreamCroissantChoppedStrawberries,
                     "CROISSANT-DISH-ICE_CREAM" -> iceCreamCroissant,

                     "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH-ICE_CREAM-TART" -> blueberriesChoppedStrawberriesCroissantIceCreamTart,
                     "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH-TART" -> blueberriesChoppedStrawberriesCroissantTart,
                     "BLUEBERRIES-CHOPPED_STRAWBERRIES-DISH-ICE_CREAM-TART" -> blueberriesChoppedStrawberriesIceCreamTart,
                     "BLUEBERRIES-CROISSANT-DISH-ICE_CREAM-TART" -> blueberriesCroissantIceCreamTart,
                     "CHOPPED_STRAWBERRIES-CROISSANT-DISH-ICE_CREAM-TART" -> choppedStrawberriesCroissantIceCreamTart,
                     "BLUEBERRIES-CHOPPED_STRAWBERRIES-DISH-TART" -> blueberriesChoppedStrawberriesCroissantIceCreamTart,
                     "BLUEBERRIES-CROISSANT-DISH-TART" -> blueberriesCroissantTart,
                     "CHOPPED_STRAWBERRIES-CROISSANT-DISH-TART" -> choppedStrawberriesCroissantTart,
                     "BLUEBERRIES-DISH-ICE_CREAM-TART" -> blueberriesIceCreamTart,
                     "CHOPPED_STRAWBERRIES-DISH-ICE_CREAM-TART" -> choppedStrawberriesIceCreamTart,
                     "CROISSANT-DISH-ICE_CREAM-TART" -> croissantIceCreamTart,
                     "BLUEBERRIES-DISH-TART" -> blueberriesTart,
                     "CROISSANT-DISH-TART" -> croissantTart,
                     "CHOPPED_STRAWBERRIES-DISH-TART" -> choppedStrawberriesTart,


  )

  implicit def toNumber(point: Point): Int = point._2 * 11 + point._1 % 11
  def toString(point: Point): String = if (point._1 < 0) "" else s"${point._1} ${point._2}"
  def toMatrix(number: Int): Point = (number % 11, number / 11)
  def walk(matrix: List[String], row: Int, col: Int) = row > 0 && row < 7 && col > 0 && col < 11 && (matrix(row)(col) == '.' || matrix(row)(col) == '0')
  def isEmptyTable(matrix: List[String], row: Int, col: Int) = row >= 0 && row < 7 && col >= 0 && col < 11 && (matrix(row)(col) == '#')
  def replaceSym(matrix: List[String], oldSym: Char, newSym: Char) = matrix.map(_.replace(oldSym, newSym))
  def cookBookKey(order: String) = order.split('-').sorted.reduce(_ + "-" + _)
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

  def nextTarget(myState: String, customerItems: List[Array[String]], cookBook: Map[String, Map[String, String]]) = {
//    customerItems.foreach(item => Console.err.println(s"customerItem: ${item(0)} - ${item(1)}\thave: $myState"))
    val cookBookData = customerItems.map(orderData =>  (orderData(0), cookBookKey(orderData(0)))).filter {
      pair => cookBook.getOrElse(pair._2, Map.empty[String, String]).contains(myState) /*&& !pair._2.contains("TART")*/
    }.sortBy(_._2)
    val properOrder = cookBookData.headOption.getOrElse("NO SUCH ORDER!", "START A NEW ONE!")

    Console.err.println(s"want to cook: ${properOrder._1}\tkey: ${properOrder._2} have: $myState")
    val recipe = cookBook.getOrElse(properOrder._2, Map.empty[String, String])
    Console.err.println(s"recipe is empty: ${recipe.isEmpty}\tmyState: $myState")
    recipe.getOrElse(myState, "TABLE")
  }

  val numallcustomers = readInt
//   Console.err.println(s"numallcustomers: $numallcustomers")

  val customerData = (for (i <- 0 until numallcustomers) yield readLine split " ").toList
  customerData.foreach(cd => Console.err.println(s"\tcustomerData: ${cd.mkString("[", " -> ", "]")}"))

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
  // (target, state)
  val stateBook = Map(
                      ("OVEN", "DOUGH") -> "-WAITING-FOR-CROISSANT",
                      ("STRAWBERRIES", "NONE-WAITING-FOR-CROISSANT") -> "-WAITING-FOR-CROISSANT",
                      ("DISH", "NONE-WAITING-FOR-CROISSANT") -> "-WAITING-FOR-CROISSANT",
                      ("DISH", "NONE-CROISSANT-IS-READY") -> "-CROISSANT-IS-READY",
                      ("CHOPPING", "STRAWBERRIES-WAITING-FOR-CROISSANT") -> "-WAITING-FOR-CROISSANT",

                      ("TABLE-STRAWBERRIES", "DISH-CROISSANT") -> "-CROISSANT-ON-A-TABLE",
                      ("STRAWBERRIES", "NONE-CROISSANT-ON-A-TABLE") -> "-CROISSANT-ON-A-TABLE",
                      ("DISH-CROISSANT", "CHOPPED_STRAWBERRIES-CROISSANT-ON-A-TABLE") -> "-CHOPPED_STRAWBERRIES-CROISSANT-ON-A-TABLE",
                      ("CHOPPING", "STRAWBERRIES-CROISSANT-ON-A-TABLE") -> "-CROISSANT-ON-A-TABLE",

                      ("OVEN", "RAW_TART") -> "-WAITING-FOR-TART",
                      ("STRAWBERRIES", "NONE-WAITING-FOR-TART") -> "-WAITING-FOR-TART",
                      ("DISH", "NONE-WAITING-FOR-TART") -> "-WAITING-FOR-TART",
                      ("DISH", "NONE-TART-IS-READY") -> "-TART-IS-READY",
                      ("CHOPPING", "STRAWBERRIES-WAITING-FOR-TART") -> "-WAITING-FOR-TART",

                      ("TABLE-DOUGH", "DISH-TART") -> "-TART-ON-A-TABLE",
                      ("DOUGH", "NONE-TART-ON-A-TABLE") -> "-TART-ON-A-TABLE",
                      ("OVEN", "NONE-TART-ON-A-TABLE-CROISSANT-IS-READY") -> "-TART-ON-A-TABLE",
                      ("OVEN", "DOUGH-TART-ON-A-TABLE") -> "-TART-ON-A-TABLE-WAITING-FOR-CROISSANT",
                      ("DISH-TART", "CROISSANT-TART-ON-A-TABLE") -> "-TART-CROISSANT-ON-A-TABLE"
  )

  def newState(prevPlayeritem: String, playeritem: String, prevTarget: String, myState: String) = {
//    stateBook.foreach(aa => Console.err.println(s"stateBook: $aa"))
    Console.err.println(s"\tprevPlayeritem: $prevPlayeritem newPlayeitem: $playeritem\n\tprevState: $prevState prevTarget: $prevTarget stateCandidate: $myState" +
      s"\tstateBook contains: ${stateBook.contains((prevTarget, myState))}")
    cookBookKey(playeritem) + stateBook.getOrElse((prevTarget, myState), "")
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
//     Console.err.println(s"numtableswithitems: $numtableswithitems")
    for (i <- 0 until numtableswithitems) {
      val Array(_tablex, _tabley, item) = readLine split " "
      val tablex = _tablex.toInt
      val tabley = _tabley.toInt
//      Console.err.println(s"\t($tablex $tabley) - $item")
      dynamicMap = addToMap(dynamicMap, item, (tablex, tabley))
    }
    // ovencontents: ignore until wood 1 league
    val Array(ovencontents, _oventimer) = readLine split " "
    dynamicMap = addToMap(dynamicMap, ovencontents, searchSym(graphMatrix, 'O').get)
//    dynamicMap.foreach(d => Console.err.println(s"Dynamic map content: ${d._1} -> ${d._2}"))
    if (ovencontents == "CROISSANT") myState = myState.replace("-WAITING-FOR-CROISSANT", "-CROISSANT-IS-READY")
    if (ovencontents == "TART") myState = myState.replace("-WAITING-FOR-TART", "-TART-IS-READY")
    if (ovencontents == "NONE") myState = myState.replace("-CROISSANT-IS-READY", "").replace("-TART-IS-READY", "")

    val oventimer = _oventimer.toInt
//   Console.err.println(s"ovencontents: $ovencontents, oventimer: $oventimer")
    val numcustomers = readInt // the number of customers currently waiting for food
    // Console.err.println(s"numcustomers: $numcustomers")
    val customers = (for (i <- 0 until numcustomers) yield readLine split " ").toList.sortBy(_(1))
    customers.foreach(c => Console.err.println(s"customeritem: ${c(0)}, customeraward: ${c(1)}"))
    val trg = nextTarget(myState, customers, cookBook)
//    Console.err.println(s"trg: $trg")
    val action = reach(graphMatrix, trg, (playerx, playery), dynamicMap)
    if (action._2._1 >= 0) target = /*action._2*/ trg

    println(s"${action._1} ${toString(action._2)}")
  }
}