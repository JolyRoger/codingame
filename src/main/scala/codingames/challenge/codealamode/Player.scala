package codingames.challenge.codealamode

object Player extends App {
//  case class Recipe (ingr: List[Map[String, Int]], f: List[Map[String, Int]] => List[Map[String, Int]])
  type Point = (Int, Int)
  type Recipe = Map[String, Map[String, String]]
  type Target = Map[String, List[(Int, Int)]]
  var prevState = ""
  var prevPlayeritem = ""
  var myState = prevState
  var target = ""

  val croissant = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE#WAITING-FOR-CROISSANT" -> "DISH",
    "DISH#WAITING-FOR-CROISSANT" -> "MOVE@OVEN",
    "NONE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT" -> "DISH",
    "DISH#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-DISH" -> "WINDOW"
  )
  val choppedStrawberries = Map("NONE" -> "STRAWBERRIES",
    "STRAWBERRIES" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES" -> "DISH"
  )
  val iceCreamBlueberries = Map("NONE" -> "DISH", "DISH" -> "BLUEBERRIES", "BLUEBERRIES-DISH" -> "ICE_CREAM", "DISH-ICE_CREAM" -> "BLUEBERRIES", "BLUEBERRIES-DISH-ICE_CREAM" -> "WINDOW")
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
  val blueberriesCroissantChoppedStrawberries = Map("NONE" -> "DOUGH",  // correct
    "DOUGH" -> "OVEN",
    "NONE#WAITING-FOR-CROISSANT" -> "DISH",
    "DISH#WAITING-FOR-CROISSANT" -> "MOVE@OVEN",
    "NONE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT" -> "DISH",
    "DISH#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-DISH" -> "TABLE@STRAWBERRIES",
    "NONE#CROISSANT-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT",
    "DISH-CROISSANT-CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT-CHOPPED_STRAWBERRIES",
    "NONE#CROISSANT-CHOPPED_STRAWBERRIES-ON-A-TABLE" -> "DISH-CROISSANT-CHOPPED_STRAWBERRIES",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> "BLUEBERRIES",
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> "WINDOW"
    )
  val croissantChoppedStrawberries= Map("NONE" -> "DOUGH",  // check
    "DOUGH" -> "OVEN",
    "NONE#WAITING-FOR-CROISSANT" -> "DISH",
    "DISH#WAITING-FOR-CROISSANT" -> "MOVE@OVEN",
    "NONE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT" -> "DISH",
    "DISH#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-DISH" -> "TABLE@STRAWBERRIES",
    "NONE#CROISSANT-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT",
    "DISH-CROISSANT-CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT-CHOPPED_STRAWBERRIES",
    "NONE#CROISSANT-CHOPPED_STRAWBERRIES-ON-A-TABLE" -> "DISH-CROISSANT-CHOPPED_STRAWBERRIES",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> "WINDOW"
  )
  val croissantBlueberries = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE#WAITING-FOR-CROISSANT" -> "DISH",
    "DISH#WAITING-FOR-CROISSANT" -> "MOVE@OVEN",
    "NONE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT" -> "DISH",
    "DISH#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-DISH" -> "BLUEBERRIES",
    "BLUEBERRIES-CROISSANT-DISH" -> "WINDOW"
  )
  val croissantBlueberriesIcecream = Map("NONE" -> "DOUGH", // check
    "DOUGH" -> "OVEN",
    "NONE#WAITING-FOR-CROISSANT" -> "DISH",
    "DISH#WAITING-FOR-CROISSANT" -> "MOVE@OVEN",
    "NONE#CROISSANT-IS-READY" -> "OVEN",
    "DISH#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT" -> "DISH",
    "CROISSANT-DISH" -> "BLUEBERRIES",
    "BLUEBERRIES-CROISSANT-DISH" -> "ICE_CREAM",
    "BLUEBERRIES-CROISSANT-DISH-ICE_CREAM" -> "WINDOW"
  )
  val iceCreamCroissant = Map("NONE" -> "DOUGH",
    "DOUGH" -> "OVEN",
    "NONE#WAITING-FOR-CROISSANT" -> "DISH",
    "DISH#WAITING-FOR-CROISSANT" -> "MOVE@OVEN",
    "NONE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT" -> "DISH",
    "DISH#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-DISH" -> "ICE_CREAM",
    "CROISSANT-DISH-ICE_CREAM" -> "WINDOW",
  )
  val iceCreamCroissantChoppedStrawberries = Map("NONE" -> "DOUGH", // correct
    "DOUGH" -> "OVEN",
    "NONE#WAITING-FOR-CROISSANT" -> "DISH",
    "DISH#WAITING-FOR-CROISSANT" -> "MOVE@OVEN",
    "NONE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT" -> "DISH",
    "DISH#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-DISH" -> "TABLE@STRAWBERRIES",
    "NONE#CROISSANT-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT",
    "DISH-CROISSANT-CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT-CHOPPED_STRAWBERRIES",
    "NONE#CROISSANT-CHOPPED_STRAWBERRIES-ON-A-TABLE" -> "DISH-CROISSANT-CHOPPED_STRAWBERRIES",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> "ICE_CREAM",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH-ICE_CREAM" -> "WINDOW"
  )
  val iceCreamBlueberriesCroissantChoppedStrawberries = Map("NONE" -> "DOUGH",  // correct
    "DOUGH" -> "OVEN",
    "NONE#WAITING-FOR-CROISSANT" -> "DISH",
    "DISH#WAITING-FOR-CROISSANT" -> "MOVE@OVEN",
    "NONE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT" -> "DISH",
    "DISH#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT-DISH" -> "TABLE@STRAWBERRIES",
    "NONE#CROISSANT-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT",
    "DISH-CROISSANT-CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "DISH-CROISSANT-CHOPPED_STRAWBERRIES",
    "NONE#CROISSANT-CHOPPED_STRAWBERRIES-ON-A-TABLE" -> "DISH-CROISSANT-CHOPPED_STRAWBERRIES",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> "BLUEBERRIES",
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH" -> "ICE_CREAM",
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH-ICE_CREAM" -> "WINDOW"
  )

  val blueberriesChoppedStrawberriesCroissantIceCreamTart = Map("NONE" -> "DOUGH")
  val blueberriesChoppedStrawberriesIceCreamTart = Map(
    "NONE" -> "DOUGH",
    "DOUGH" -> "CHOPPING",
    "CHOPPED_DOUGH" -> "BLUEBERRIES",
    "RAW_TART" -> "OVEN",
    "NONE#WAITING-FOR-TART" -> "DISH",
    "NONE#TART-IS-READY" -> "DISH",
    "DISH#WAITING-FOR-TART" -> "MOVE@OVEN",
    "DISH#TART-IS-READY" -> "OVEN",
    "DISH-TART" -> "TABLE@STRAWBERRIES",
    "NONE#TART-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES#TART-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES#TART-ON-A-TABLE" -> "DISH-TART",
    "NONE#TART-CHOPPED_STRAWBERRIES-ON-A-TABLE" -> "DISH-TART-CHOPPED_STRAWBERRIES",
    "CHOPPED_STRAWBERRIES-DISH-TART" -> "BLUEBERRIES",
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-DISH-TART" -> "ICE_CREAM",
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-DISH-ICE_CREAM-TART" -> "WINDOW"
  )
  def blueberriesChoppedStrawberriesCroissantTart = Map(    // correct
    "NONE" -> "DOUGH",
    "DOUGH" -> "CHOPPING",
    "CHOPPED_DOUGH" -> "BLUEBERRIES",
    "RAW_TART" -> "OVEN",
    "NONE#WAITING-FOR-TART" -> "DISH",
    "NONE#TART-IS-READY" -> "DISH",
    "DISH#WAITING-FOR-TART" -> "MOVE@OVEN",
    "DISH#TART-IS-READY" -> "OVEN",
    "DISH-TART" -> "TABLE@DOUGH",
    "NONE#TART-ON-A-TABLE" -> "DOUGH",
    "DOUGH#TART-ON-A-TABLE" -> "OVEN",
    "NONE#TART-ON-A-TABLE#WAITING-FOR-CROISSANT" -> "WAIT",
    "NONE#TART-ON-A-TABLE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT#TART-ON-A-TABLE" -> "DISH-TART",
    "NONE#TART-CROISSANT-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES#TART-CROISSANT-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES#TART-CROISSANT-ON-A-TABLE" -> "DISH-TART-CROISSANT",
    "NONE#TART-CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "DISH-TART-CROISSANT-CHOPPED_STRAWBERRIES",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH-TART" -> "BLUEBERRIES",
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-CROISSANT-DISH-TART" -> "WINDOW"
  )
  val blueberriesCroissantIceCreamTart = Map( // correct
    "NONE" -> "DOUGH",
    "DOUGH" -> "CHOPPING",
    "CHOPPED_DOUGH" -> "BLUEBERRIES",
    "RAW_TART" -> "OVEN",
    "NONE#WAITING-FOR-TART" -> "DISH",
    "NONE#TART-IS-READY" -> "DISH",
    "DISH#WAITING-FOR-TART" -> "MOVE@OVEN",
    "DISH#TART-IS-READY" -> "OVEN",
    "DISH-TART" -> "TABLE@DOUGH",
    "NONE#TART-ON-A-TABLE" -> "DOUGH",
    "DOUGH#TART-ON-A-TABLE" -> "OVEN",
    "NONE#TART-ON-A-TABLE#WAITING-FOR-CROISSANT" -> "WAIT",
    "NONE#TART-ON-A-TABLE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT#TART-ON-A-TABLE" -> "DISH-TART",
    "NONE#TART-CROISSANT-ON-A-TABLE" -> "DISH-TART-CROISSANT",
    "CROISSANT-DISH-TART" -> "BLUEBERRIES",
    "BLUEBERRIES-CROISSANT-DISH-TART" -> "ICE_CREAM",
    "BLUEBERRIES-CROISSANT-DISH-ICE_CREAM-TART" -> "WINDOW"
  )
  val choppedStrawberriesCroissantIceCreamTart = Map(
    "NONE" -> "DOUGH",
    "DOUGH" -> "CHOPPING",
    "CHOPPED_DOUGH" -> "BLUEBERRIES",
    "RAW_TART" -> "OVEN",
    "NONE#WAITING-FOR-TART" -> "DISH",
    "NONE#TART-IS-READY" -> "DISH",
    "DISH#WAITING-FOR-TART" -> "MOVE@OVEN",
    "DISH#TART-IS-READY" -> "OVEN",
    "DISH-TART" -> "TABLE@DOUGH",
    "NONE#TART-ON-A-TABLE" -> "DOUGH",
    "DOUGH#TART-ON-A-TABLE" -> "OVEN",
    "NONE#TART-ON-A-TABLE#WAITING-FOR-CROISSANT" -> "WAIT",
    "NONE#TART-ON-A-TABLE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT#TART-ON-A-TABLE" -> "DISH-TART",
    "NONE#TART-CROISSANT-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES#TART-CROISSANT-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES#TART-CROISSANT-ON-A-TABLE" -> "DISH-TART-CROISSANT",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH-TART" -> "ICE_CREAM",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH-ICE_CREAM-TART" -> "WINDOW"
  )
  val blueberriesChoppedStrawberriesTart = Map(   // check
    "NONE" -> "DOUGH",
    "DOUGH" -> "CHOPPING",
    "CHOPPED_DOUGH" -> "BLUEBERRIES",
    "RAW_TART" -> "OVEN",
    "NONE#WAITING-FOR-TART" -> "DISH",
    "NONE#TART-IS-READY" -> "DISH",
    "DISH#WAITING-FOR-TART" -> "MOVE@OVEN",
    "DISH#TART-IS-READY" -> "OVEN",
    "DISH-TART" -> "TABLE@STRAWBERRIES",
    "NONE#TART-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES#TART-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES#TART-ON-A-TABLE" -> "DISH-TART",
    "NONE#TART-CHOPPED_STRAWBERRIES-ON-A-TABLE" -> "DISH-TART-CHOPPED_STRAWBERRIES",
    "CHOPPED_STRAWBERRIES-DISH-TART" -> "BLUEBERRIES",
    "BLUEBERRIES-CHOPPED_STRAWBERRIES-DISH-TART" -> "WINDOW"
  )
  val blueberriesCroissantTart = Map(
    "NONE" -> "DOUGH",
    "DOUGH" -> "CHOPPING",
    "CHOPPED_DOUGH" -> "BLUEBERRIES",
    "RAW_TART" -> "OVEN",
    "NONE#WAITING-FOR-TART" -> "DISH",
    "NONE#TART-IS-READY" -> "DISH",
    "DISH#WAITING-FOR-TART" -> "MOVE@OVEN",
    "DISH#TART-IS-READY" -> "OVEN",
    "DISH-TART" -> "TABLE@DOUGH",
    "NONE#TART-ON-A-TABLE" -> "DOUGH",
    "DOUGH#TART-ON-A-TABLE" -> "OVEN",
    "DOUGH#TART-ON-A-TABLE" -> "OVEN",
    "NONE#TART-ON-A-TABLE#WAITING-FOR-CROISSANT" -> "WAIT",
    "NONE#TART-ON-A-TABLE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT#TART-ON-A-TABLE" -> "DISH-TART",
    "NONE#TART-CROISSANT-ON-A-TABLE" -> "DISH-TART-CROISSANT",
    "CROISSANT-DISH-TART" -> "BLUEBERRIES",
    "BLUEBERRIES-CROISSANT-DISH-TART" -> "WINDOW"
  )
  val choppedStrawberriesCroissantTart = Map(   // check
    "NONE" -> "DOUGH",
    "DOUGH" -> "CHOPPING",
    "CHOPPED_DOUGH" -> "BLUEBERRIES",
    "RAW_TART" -> "OVEN",
    "NONE#WAITING-FOR-TART" -> "DISH",
    "NONE#TART-IS-READY" -> "DISH",
    "DISH#WAITING-FOR-TART" -> "MOVE@OVEN",
    "DISH#TART-IS-READY" -> "OVEN",
    "DISH-TART" -> "TABLE@DOUGH",
    "NONE#TART-ON-A-TABLE" -> "DOUGH",
    "DOUGH#TART-ON-A-TABLE" -> "OVEN",
    "NONE#TART-ON-A-TABLE#WAITING-FOR-CROISSANT" -> "WAIT",
    "NONE#TART-ON-A-TABLE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT#TART-ON-A-TABLE" -> "DISH-TART",
    "NONE#TART-CROISSANT-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES#TART-CROISSANT-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES#TART-CROISSANT-ON-A-TABLE" -> "DISH-TART-CROISSANT",
    "NONE#TART-CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE" -> "DISH-TART-CROISSANT-CHOPPED_STRAWBERRIES",
    "CHOPPED_STRAWBERRIES-CROISSANT-DISH-TART" -> "WINDOW"
  )
  val blueberriesIceCreamTart = Map("NONE" -> "DOUGH")
  val choppedStrawberriesIceCreamTart = Map("NONE" -> "DOUGH")
  val croissantIceCreamTart = Map("NONE" -> "DOUGH",
    "DOUGH" -> "CHOPPING",
    "CHOPPED_DOUGH" -> "BLUEBERRIES",
    "RAW_TART" -> "OVEN",
    "NONE#WAITING-FOR-TART" -> "DISH",
    "NONE#TART-IS-READY" -> "DISH",
    "DISH#WAITING-FOR-TART" -> "MOVE@OVEN",
    "DISH#TART-IS-READY" -> "OVEN",
    "DISH-TART" -> "TABLE@DOUGH",
    "NONE#TART-ON-A-TABLE" -> "DOUGH",
    "DOUGH#TART-ON-A-TABLE" -> "OVEN",
    "DOUGH#TART-ON-A-TABLE" -> "OVEN",
    "NONE#TART-ON-A-TABLE#WAITING-FOR-CROISSANT" -> "WAIT",
    "NONE#TART-ON-A-TABLE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT#TART-ON-A-TABLE" -> "DISH-TART",
    "NONE#TART-CROISSANT-ON-A-TABLE" -> "DISH-TART-CROISSANT",
    "CROISSANT-DISH-TART" -> "ICE_CREAM",
    "CROISSANT-DISH-ICE_CREAM-TART" -> "WINDOW")
  val blueberriesTart = Map("NONE" -> "DOUGH",  // correct
    "DOUGH" -> "CHOPPING",
    "CHOPPED_DOUGH" -> "BLUEBERRIES",
    "RAW_TART" -> "OVEN",
    "NONE#WAITING-FOR-TART" -> "DISH",
    "NONE#TART-IS-READY" -> "DISH",
    "DISH#WAITING-FOR-TART" -> "MOVE@OVEN",
    "DISH#TART-IS-READY" -> "OVEN",
    "DISH-TART" -> "BLUEBERRIES",
    "BLUEBERRIES-DISH-TART" -> "WINDOW"
  )
  def croissantTart = Map("NONE" -> "DOUGH",
    "DOUGH" -> "CHOPPING",
    "CHOPPED_DOUGH" -> "BLUEBERRIES",
    "RAW_TART" -> "OVEN",
    "NONE#WAITING-FOR-TART" -> "DISH",
    "NONE#TART-IS-READY" -> "DISH",
    "DISH#WAITING-FOR-TART" -> "MOVE@OVEN",
    "DISH#TART-IS-READY" -> "OVEN",
    "DISH-TART" -> "TABLE@DOUGH",
    "NONE#TART-ON-A-TABLE" -> "DOUGH",
    "DOUGH#TART-ON-A-TABLE" -> "OVEN",
    "DOUGH#TART-ON-A-TABLE" -> "OVEN",
    "NONE#TART-ON-A-TABLE#WAITING-FOR-CROISSANT" -> "WAIT",
    "NONE#TART-ON-A-TABLE#CROISSANT-IS-READY" -> "OVEN",
    "CROISSANT#TART-ON-A-TABLE" -> "DISH-TART",
    "NONE#TART-CROISSANT-ON-A-TABLE" -> "DISH-TART-CROISSANT",
    "CROISSANT-DISH-TART" -> "WINDOW"
  )
  val choppedStrawberriesTart = Map(  // check
    "NONE" -> "DOUGH",
    "DOUGH" -> "CHOPPING",
    "CHOPPED_DOUGH" -> "BLUEBERRIES",
    "RAW_TART" -> "OVEN",
    "NONE#WAITING-FOR-TART" -> "DISH",
    "NONE#TART-IS-READY" -> "DISH",
    "DISH#WAITING-FOR-TART" -> "MOVE@OVEN",
    "DISH#TART-IS-READY" -> "OVEN",
    "DISH-TART" -> "TABLE@STRAWBERRIES",
    "NONE#TART-ON-A-TABLE" -> "STRAWBERRIES",
    "STRAWBERRIES#TART-ON-A-TABLE" -> "CHOPPING",
    "CHOPPED_STRAWBERRIES#TART-ON-A-TABLE" -> "DISH-TART",
    "CHOPPED_STRAWBERRIES-DISH-TART" -> "WINDOW"
  )

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
                     "BLUEBERRIES-CHOPPED_STRAWBERRIES-DISH-TART" -> blueberriesChoppedStrawberriesTart,
                     "BLUEBERRIES-CROISSANT-DISH-TART" -> blueberriesCroissantTart,
                     "CHOPPED_STRAWBERRIES-CROISSANT-DISH-TART" -> choppedStrawberriesCroissantTart,
                     "BLUEBERRIES-DISH-ICE_CREAM-TART" -> blueberriesIceCreamTart,
                     "CHOPPED_STRAWBERRIES-DISH-ICE_CREAM-TART" -> choppedStrawberriesIceCreamTart,
                     "CROISSANT-DISH-ICE_CREAM-TART" -> croissantIceCreamTart,
                     "BLUEBERRIES-DISH-TART" -> blueberriesTart,
                     "CROISSANT-DISH-TART" -> croissantTart,
                     "CHOPPED_STRAWBERRIES-DISH-TART" -> choppedStrawberriesTart


  )

  implicit def toNumber(point: Point): Int = point._2 * 11 + point._1 % 11
  def toString(point: Point): String = if (point._1 < 0) "" else s"${point._1} ${point._2}"
  def toMatrix(number: Int): Point = (number % 11, number / 11)
  def walk(matrix: List[String], row: Int, col: Int) = row > 0 && row < 7 && col > 0 && col < 11 && (matrix(row)(col) == '.' || matrix(row)(col) == '0')
  def isEmptyTable(matrix: List[String], row: Int, col: Int) = row >= 0 && row < 7 && col >= 0 && col < 11 && (matrix(row)(col) == '#')
  def replaceSym(matrix: List[String], oldSym: Char, newSym: Char) = matrix.map(_.replace(oldSym, newSym))
  def cookBookKey(order: String) = order.split('-').sorted.reduce(_ + "-" + _)
  def searchClosestEmptyTable(matrix: List[String],  dynamicMap: Map[String, List[Point]], point: Point, diameter: Int): Point = {
    Console.err.println(s"CLOSEST EMPTY TABLE OF $point")
    List((point._1 - diameter, point._2 - diameter), (point._1, point._2 - diameter),
      (point._1 + diameter, point._2 - diameter), (point._1 - diameter, point._2), (point._1 + diameter, point._2), (point._1 - diameter, point._2 + diameter),
      (point._1, point._2 + diameter), (point._1 + diameter, point._2 + diameter)).find(p => isEmptyTable(matrix, p._2, p._1) && !dynamicMap.values.exists(_.contains(p))).getOrElse(
        searchClosestEmptyTable(matrix, dynamicMap, point, diameter + 1))
  }

  def reach(graphMatrix: List[String], target: String, from: Point, dynamicMap: Target) = {
    Console.err.println(s"TARGETPOINT::target: $target")
    if (target == "TABLE") ("USE", searchClosestEmptyTable(graphMatrix, dynamicMap, from, 1))
    else if (target.startsWith("MOVE@")) ("MOVE", dynamicMap.getOrElse(target.split('@')(1), List(searchClosestEmptyTable(graphMatrix, dynamicMap, from, 1))).head)
    else if (target.startsWith("TABLE@")) ("USE", searchClosestEmptyTable(graphMatrix, dynamicMap, dynamicMap.getOrElse(target.split('@')(1), List(
        searchClosestEmptyTable(graphMatrix, dynamicMap, from, 1))).head, 1))
    else if (target == "WAIT") ("WAIT", (-1,-1)) else ("USE", dynamicMap.getOrElse(target, List(searchClosestEmptyTable(graphMatrix, dynamicMap, from, 1))).head)
  }

  def searchSym(matrix: List[String], sym: Char) = {
    val coords = for (row <- matrix.indices; col <- matrix(row).indices; _sym = matrix(row)(col); if _sym == sym) yield (col, row)
    if (coords.nonEmpty) Some(coords(0)) else None
  }
  def adjTo(matrix: List[String], point: Point): List[Point] = List((point._1 - 1, point._2 - 1), (point._1, point._2 - 1),
                   (point._1 + 1, point._2 - 1), (point._1 - 1, point._2), (point._1 + 1, point._2), (point._1 - 1, point._2 + 1),
                   (point._1, point._2 + 1), (point._1 + 1, point._2 + 1)).filter(p => walk(matrix, p._2, p._1))

  def getTargetFromRecipeBook(recipe: Map[String, String], targetMap: Target, myState: String) = recipe.get(myState) match {
      case Some(newState) =>
        if (targetMap.keys.exists { key =>
          (if (newState.startsWith("TABLE@") || newState.startsWith("MOVE@")) newState.split("@")(1) else newState) == key
        }) newState else "TABLE"
        //        targetMap.keys.find(key => (if (newState.startsWith("TABLE@") || newState.startsWith("MOVE@")) newState.split("@")(1) else newState) == key).getOrElse("TABLE")
      case None => "TABLE"
    }

  def getTargetFromGameField(myState: String, customerItems: List[Array[String]], cookBook: Recipe, targetStaticMap: Target, targetMap: Target,
                             recipe: Map[String, String], customerOrdersData: List[(String, String)]) = {
    val customerOrdersDataMap = customerOrdersData.toMap

    val targetsOnTables = targetMap.keys.filterNot(tmKey => targetStaticMap.keys.exists(_ == tmKey))
    val dishes = targetsOnTables.filter(_.startsWith("DISH-"))
    val mayCook: List[(String, List[String])] = customerOrdersData.map(cod => {
      val codIngredients = cod._1.split("-")
      val totIngredients = targetsOnTables.map(tot => (tot.split("-"), tot)).toMap
      (cod._1, totIngredients.keys.filter(ingr => ingr.forall(codIngredients.contains(_))).map(totIngredients(_)).toList.sortWith(
        _.split("-").length > _.split("-").length
      ))
    }).filterNot(_._2.isEmpty)


    mayCook.foreach(cook => {
      Console.err.println(s"May cook: ${cook._1} -> ${cook._2.mkString("[", ", ", "]")}")
    })
    if (mayCook.isEmpty) {
      val dishesOnTables = targetMap.keys.filter(tmKey => tmKey.startsWith("DISH-"))
      val dishFromMyRecipe = dishesOnTables.find(dish => {
        recipe.keys.exists(_ == customerOrdersDataMap.getOrElse(dish, "")) &&
          recipe(customerOrdersDataMap.getOrElse(dish, "")) == "WINDOW"
      })
      dishFromMyRecipe
    } else {
      Some(mayCook.head._1)
    }
  }

  def getTargetToReadyDish(player: Point, state: String, dish: String, targetMap: Target) = {
    val target = targetMap(dish).head
    if (Math.max(player._1, target._1) - Math.min(player._1, target._1)  <= 2 && Math.max(player._2, target._2) - Math.min(player._2, target._2) <= 2) {
      (if(state.startsWith("NONE")) ""  else "TABLE@") + dish
    } else "MOVE@" + dish
  }


  def nextTarget(myState: String, customerItems: List[Array[String]], cookBook: Recipe, targetMap: Target, player: Point) = {
//    customerItems.foreach(item => Console.err.println(s"customerItem: ${item(0)} - ${item(1)}\thave: $myState"))
    val customerOrdersData = customerItems.map(orderData =>  (orderData(0), cookBookKey(orderData(0))))/*.filter {
      pair => cookBook.getOrElse(pair._2, Map.empty[String, String]).contains(myState) /*&& !pair._2.contains("TART")*/
    }*/.sortBy(_._2)
    val properOrder = customerOrdersData.headOption.getOrElse("NO SUCH ORDER!", "START A NEW ONE!")

    Console.err.println(s"want to cook: ${properOrder._1}\tkey: ${properOrder._2} have: $myState")
    val recipe = cookBook.getOrElse(properOrder._2, Map.empty[String, String])
    Console.err.println(s"recipe is empty: ${recipe.isEmpty}\tmyState: $myState")
    getTargetFromGameField(myState: String, customerItems: List[Array[String]], cookBook: Recipe, targetStaticMap, targetMap, recipe, customerOrdersData) match {
      case Some(dish) => getTargetToReadyDish(player, myState, dish, targetMap)
      case None => getTargetFromRecipeBook(recipe, targetMap, myState)
    }
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

  def addToMap(oldMap: Target, key: String, value: (Int, Int)) =
    oldMap + (key -> (oldMap.get(key) match {
      case Some(lst) => value :: lst
      case None => List(value)
    }))
  // (target, state)
  def stateBook = Map(
                      ("OVEN", "DOUGH") -> "#WAITING-FOR-CROISSANT",
/*
                      ("STRAWBERRIES", "NONE#WAITING-FOR-CROISSANT") -> "#WAITING-FOR-CROISSANT",
                      ("DISH", "NONE#WAITING-FOR-CROISSANT") -> "#WAITING-FOR-CROISSANT",
                      ("DISH", "NONE#CROISSANT-IS-READY") -> "#CROISSANT-IS-READY",
                      ("CHOPPING", "STRAWBERRIES#WAITING-FOR-CROISSANT") -> "#WAITING-FOR-CROISSANT",

                      ("TABLE@STRAWBERRIES", "DISH-CROISSANT") -> "#CROISSANT-ON-A-TABLE",
                      ("STRAWBERRIES", "NONE#CROISSANT-ON-A-TABLE") -> "#CROISSANT-ON-A-TABLE",
                      ("DISH-CROISSANT", "CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE") -> "#CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE",
                      ("CHOPPING", "STRAWBERRIES#CROISSANT-ON-A-TABLE") -> "#CROISSANT-ON-A-TABLE",
*/

                      ("OVEN", "RAW_TART") -> "#WAITING-FOR-TART",
                      ("TABLE@DOUGH", "DISH-TART") -> "#TART-ON-A-TABLE",
                      ("TABLE@STRAWBERRIES", "DISH-TART") -> "#TART-ON-A-TABLE",
                      ("TABLE@STRAWBERRIES", "CROISSANT-DISH") -> "#CROISSANT-ON-A-TABLE",
                      ("OVEN", "DOUGH#TART-ON-A-TABLE") -> "#TART-ON-A-TABLE#WAITING-FOR-CROISSANT",
                      ("DISH-TART", "CROISSANT#TART-ON-A-TABLE") -> "#TART-CROISSANT-ON-A-TABLE",
                      ("DISH-CROISSANT", "CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE") -> "#CROISSANT-CHOPPED_STRAWBERRIES-ON-A-TABLE",
                      ("DISH-TART", "CHOPPED_STRAWBERRIES#TART-ON-A-TABLE") -> "#TART-CHOPPED_STRAWBERRIES-ON-A-TABLE",
                      ("DISH-TART-CROISSANT", "NONE#TART-CROISSANT-ON-A-TABLE") -> "",
                      ("DISH-TART-CHOPPED_STRAWBERRIES", "NONE#TART-CHOPPED_STRAWBERRIES-ON-A-TABLE") -> "",
                      ("OVEN", "DISH#TART-IS-READY") -> "",
                      ("OVEN", "DISH#CROISSANT-IS-READY") -> "",
                      ("OVEN", "NONE#TART-IS-READY") -> "",
                      ("OVEN", "NONE#CROISSANT-IS-READY") -> "",
                      ("OVEN", "NONE#TART-ON-A-TABLE#CROISSANT-IS-READY") -> "#TART-ON-A-TABLE",
                      ("OVEN", "DOUGH#TART-ON-A-TABLE") -> "#TART-ON-A-TABLE#WAITING-FOR-CROISSANT",
                      ("DISH-TART-CROISSANT", "CHOPPED_STRAWBERRIES#TART-CROISSANT-ON-A-TABLE") -> "#TART-CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE",
                      ("DISH-TART-CROISSANT-CHOPPED_STRAWBERRIES", "NONE#TART-CHOPPED_STRAWBERRIES#CROISSANT-ON-A-TABLE") -> "",
                      ("DISH-CROISSANT-CHOPPED_STRAWBERRIES", "NONE#CROISSANT-CHOPPED_STRAWBERRIES-ON-A-TABLE") -> ""
    /*"NONE#TART-ON-A-TABLE#CROISSANT-IS-READY" -> "OVEN",
                      ("STRAWBERRIES", "NONE#WAITING-FOR-TART") -> "#WAITING-FOR-TART",
                      ("DISH", "NONE#WAITING-FOR-TART") -> "#WAITING-FOR-TART",
                      ("DISH", "NONE#TART-IS-READY") -> "#TART-IS-READY",
                      ("CHOPPING", "STRAWBERRIES#WAITING-FOR-TART") -> "#WAITING-FOR-TART",

                      ("DOUGH", "NONE#TART-ON-A-TABLE") -> "#TART-ON-A-TABLE",
                      ("OVEN", "NONE#TART-ON-A-TABLE#CROISSANT-IS-READY") -> "#TART-ON-A-TABLE",
*/
  )

  def newState(prevPlayeritem: String, playeritem: String, prevTarget: String, myState: String) = {
//    stateBook.foreach(aa => Console.err.println(s"stateBook: $aa"))
    Console.err.println(s"\tprevPlayeritem: $prevPlayeritem newPlayeitem: $playeritem\n\tprevState: $prevState prevTarget: $prevTarget stateCandidate: $myState" +
      s"\tstateBook contains: ${stateBook.contains((prevTarget, myState))}")
    val stateArr = myState.split('#')
    cookBookKey(playeritem) + stateBook.getOrElse((prevTarget, myState), if (stateArr.length > 1) "#" + stateArr(1) else "")
  }

  def updateState(state: String, ovencontents: String, dynamicMap: Target) = {
    if (ovencontents == "CROISSANT") myState = myState.replace("#WAITING-FOR-CROISSANT", "#CROISSANT-IS-READY")
    if (ovencontents == "TART") myState = myState.replace("#WAITING-FOR-TART", "#TART-IS-READY")
    if (ovencontents == "DOUGH") myState = myState.replace("#CROISSANT-IS-READY", "").replace("#TART-IS-READY", "")
    if (ovencontents == "NONE") myState = myState.replace("#CROISSANT-IS-READY", "").replace("#TART-IS-READY", "").replace("#WAITING-FOR-CROISSANT", "").replace("#WAITING-FOR-TART", "")
//    if (!dynamicMap.keys.exists(_ == "DISH-TART-CROISSANT")) myState = myState.replace("#TART-CROISSANT-ON-A-TABLE", "")
//    if (!dynamicMap.keys.exists(_ == "DISH-CROISSANT")) myState = myState.replace("#CROISSANT-ON-A-TABLE", "")
//    if (!dynamicMap.keys.exists(_ == "DISH-CROISSANT-CHOPPED_STRAWBERRIES")) myState = myState.replace("#CROISSANT-CHOPPED_STRAWBERRIES-ON-A-TABLE", "")
//    if (!dynamicMap.keys.exists(_ == "DISH-TART")) myState = myState.replace("#TART-ON-A-TABLE", "")
    myState
  }


  // game loop
  while (true) {
    var dynamicMap = targetStaticMap
    val turnsremaining = readInt
     Console.err.println(s"turnsremaining: $turnsremaining")
    if (turnsremaining >= 198) myState = "NONE"
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
//    dynamicMap = addToMap(dynamicMap, ovencontents, searchSym(graphMatrix, 'O').get)
    dynamicMap = searchSym(graphMatrix, 'O') match {
      case Some(oven) => addToMap(dynamicMap, ovencontents, oven)
      case None => dynamicMap
    }
//    dynamicMap.foreach(d => Console.err.println(s"Dynamic map content: ${d._1} -> ${d._2}"))
    val oventimer = _oventimer.toInt
//   Console.err.println(s"ovencontents: $ovencontents, oventimer: $oventimer")
    val numcustomers = readInt // the number of customers currently waiting for food
    // Console.err.println(s"numcustomers: $numcustomers")
    val customers = (for (i <- 0 until numcustomers) yield readLine split " ").toList.sortBy(_(1))
    customers.foreach(c => Console.err.println(s"customeritem: ${c(0)}, customeraward: ${c(1)}"))
    val trg = nextTarget(myState, customers, cookBook, dynamicMap, (playerx, playery))
//    Console.err.println(s"trg: $trg")
    val action = reach(graphMatrix, trg, (playerx, playery), dynamicMap)

    Console.err.print(s"myState [$myState] after update ")
    myState = updateState(myState, ovencontents, dynamicMap)
    Console.err.println(s" [$myState]")

    if (action._2._1 >= 0) target = /*action._2*/ trg

    println(s"${action._1} ${toString(action._2)}")
  }
}