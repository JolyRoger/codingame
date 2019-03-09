
object MyState extends Enumeration {
  type Margin = Value
  val EMPTY, WITH_EMPTY_PLATE, WITH_BLUEBERRY_PLATE, WITH_ICECREAM_PLATE, WITH_BOTH_PLATE = Value
}

object Player extends App {
  val numallcustomers = readInt
  Console.err.println(s"numallcustomers: $numallcustomers")
  for (i <- 0 until numallcustomers) {
    // customeritem: the food the customer is waiting for
    // customeraward: the number of points awarded for delivering the food
    val Array(customeritem, _customeraward) = readLine split " "
    val customeraward = _customeraward.toInt
//    Console.err.println(s"\tcustomeritem: $customeritem customeraward: $customeraward")
  }
  var dishes: (Int, Int) = (-1, -1)
  var window: (Int, Int) = (-1, -1)
  var blueberry: (Int, Int) = (-1, -1)
  var icecream: (Int, Int) = (-1, -1)
  var myState = MyState.EMPTY

  for (i <- 0 until 7) {
    val kitchenline = readLine
    Console.err.println(s"\t\tkitchenline[$i]: $kitchenline")
    if (kitchenline.contains("D")) dishes = (kitchenline.indexOf('D'), i)
    if (kitchenline.contains("W")) window = (kitchenline.indexOf('W'), i)
    if (kitchenline.contains("B")) blueberry = (kitchenline.indexOf('B'), i)
    if (kitchenline.contains("I")) icecream = (kitchenline.indexOf('I'), i)
  }

  // game loop
  while (true) {
    val turnsremaining = readInt
    Console.err.println(s"turnsremaining: $turnsremaining")
    val Array(_playerx, _playery, playeritem) = readLine split " "
    val playerx = _playerx.toInt
    val playery = _playery.toInt
    Console.err.println(s"playerx: $playerx, playery: $playery, playeritem: $playeritem")
    val Array(_partnerx, _partnery, partneritem) = readLine split " "
    val partnerx = _partnerx.toInt
    val partnery = _partnery.toInt
    Console.err.println(s"partnerx: $partnerx, partnery: $partnery, partneritem: $partneritem")
    val numtableswithitems = readInt // the number of tables in the kitchen that currently hold an item
    Console.err.println(s"numtableswithitems: $numtableswithitems")
    for (i <- 0 until numtableswithitems) {
      val Array(_tablex, _tabley, item) = readLine split " "
      val tablex = _tablex.toInt
      val tabley = _tabley.toInt
      Console.err.println(s"\ttablex: $tablex, tabley: $tabley")
    }
    // ovencontents: ignore until wood 1 league
    val Array(ovencontents, _oventimer) = readLine split " "
    val oventimer = _oventimer.toInt
    Console.err.println(s"ovencontents: $ovencontents, oventimer: $oventimer")
    val numcustomers = readInt // the number of customers currently waiting for food
    Console.err.println(s"numcustomers: $numcustomers")
    val customers = (for (i <- 0 until numcustomers) yield readLine split " ").sortBy(_(0))
    customers.foreach(c => Console.err.println(s"customeritem: ${c(0)}, customeraward: ${c(1)}"))
//    for (i <- 0 until numcustomers) {
//      val Array(customeritem, _customeraward) = readLine split " "
//      val customeraward = _customeraward.toInt
//      Console.err.println(s"\tcustomeritem: $customeritem, customeraward: $customeraward")
//    }

    val action = "USE"
    val (coord, nextState) = if (myState == MyState.EMPTY) (dishes, MyState.WITH_EMPTY_PLATE)
                else if (myState == MyState.WITH_EMPTY_PLATE) (blueberry, MyState.WITH_BLUEBERRY_PLATE)
                else if (myState == MyState.WITH_BLUEBERRY_PLATE) (icecream, MyState.WITH_BOTH_PLATE)
                else if (myState == MyState.WITH_ICECREAM_PLATE) (blueberry, MyState.WITH_BOTH_PLATE)
                else if (myState == MyState.WITH_BOTH_PLATE) (window, MyState.EMPTY) else ((-1,-1), MyState.EMPTY)

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")


    // MOVE x y
    // USE x y
    // WAIT
    println(s"USE ${coord._1} ${coord._2}; COORDS: ${coord._1} ${coord._2}, STATE: $myState")
    myState = nextState
  }
}