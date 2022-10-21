package codingames.challenge.greencircle

import math._
import scala.io.Source
import scala.util._
import scala.io.StdIn._

/**
 * Complete the hackathon before your opponent by following the principles of Green IT
 **/
object Player extends App {
//------------------------------------------FILE ENTRY------------------------------------------------------------------
  // val filename = "resources/greencircle/2.txt"
  // val bufferedSource = Source.fromFile(filename)
  // val data = bufferedSource.getLines
  // def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
  // def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
//----------------------------------------------------------------------------------------------------------------------
  case class Application(id: Int,
                         trainingNeeded: Int,
                         codingNeeded: Int,
                         dailyRoutineNeeded: Int,
                         taskPrioritizationNeeded: Int,
                         architectureStudyNeeded: Int,
                         continuousDeliveryNeeded: Int,
                         codeReviewNeeded: Int,
                         refactoringNeeded: Int) {
    val skills = Array(trainingNeeded, codingNeeded, dailyRoutineNeeded, taskPrioritizationNeeded, architectureStudyNeeded, continuousDeliveryNeeded, codeReviewNeeded, refactoringNeeded)
    val notEmptySkills = skills.zipWithIndex.filter(indexSkill => indexSkill._1 != 0).map(_._2).toList
    val skillSum = skills.sum
    private def diff(crs: (Int, Int)) = { val diff = crs._1 - crs._2 * 2; if (diff < 0) 0 else diff }
    def applyCard(card: Card) = skills.zip(card.usefulCards).map(diff).sum - card.bonusCardsCount
  }
  case class Card(cardsLocation: String,
                  trainingCardsCount: Int,
                  codingCardsCount: Int,
                  dailyRoutineCardsCount: Int,
                  taskPrioritizationCardsCount: Int,
                  architectureStudyCardsCount: Int,
                  continuousDeliveryCardsCount: Int,
                  codeReviewCardsCount: Int,
                  refactoringCardsCount: Int,
                  bonusCardsCount: Int,
                  technicalDebtCardsCount: Int) {
    val usefulCards = Array(trainingCardsCount, codingCardsCount, dailyRoutineCardsCount, taskPrioritizationCardsCount, architectureStudyCardsCount, continuousDeliveryCardsCount, codeReviewCardsCount, refactoringCardsCount)

    def applyTask(task: (Int, Int)) = {
      val cardArr = Array(usefulCards(0), usefulCards(1), usefulCards(2), usefulCards(3), usefulCards(4), usefulCards(5), usefulCards(6), usefulCards(7), bonusCardsCount)
      cardArr(task._1) -= 1
      cardArr(task._2) += 1
      Card(cardsLocation, cardArr(0), cardArr(1), cardArr(2), cardArr(3), cardArr(4), cardArr(5), cardArr(6), cardArr(7), cardArr(8), technicalDebtCardsCount)
    }
  }
  var applications = List.empty[Application]

  // HAND card and all possible skills allowing to be taken, eg List(0, 1, 3, 4, 6)
  def findMoveApp(card: Card, moves: List[Int]) = {
    val sortedApps = applications.sortBy(app => app.applyCard(card))
    var moveOpt = Option.empty[Int]
    var appOpt = Option.empty[Application]
    for (app <- sortedApps) {
      val movesAppsIntersection = app.notEmptySkills.intersect(moves)
      if (moveOpt.isEmpty && movesAppsIntersection.nonEmpty) {
        moveOpt = movesAppsIntersection.find(_ => true)
        appOpt = Some(app)
      }
    }
    (moveOpt, appOpt)
  }
  def findRelease(card: Card, releases: List[Int]) = {
    val sortedApps = applications.sortBy(app => app.applyCard(card))
    sortedApps.headOption.map(_.id)
  }

  def calculateCard(action: Int, actions: IndexedSeq[Int], cardsMap: Map[String, IndexedSeq[Card]], apps: List[Application]): String = {
    if (actions.length < 2) s"MOVE $action" else {
      val head = cardsMap("HAND").head
      val newAppMap = apps.map(app => {
        val currentAppScore = app.applyCard(head)       // current score: how many points remains to release
        val priorityCalc = actions.map(action => {      // future score: how many points will remain after applying a card to the application
          val newCard = head.applyTask((action, 0))
          (action, app.applyCard(newCard))
        })
        val newBestAppScore = priorityCalc.minBy(_._2)   // the best score reachable for the application for this receiving action set. Minimize or maximize - it depends
        Console.err.println(s"App ${app.id} cur=$currentAppScore best=${newBestAppScore._2} after (${newBestAppScore._1})")
        (newBestAppScore._1, currentAppScore - newBestAppScore._2, newBestAppScore._2)
      }).groupBy(_._2)
      val minApps = newAppMap.minBy(_._1)._2
      val newApp = minApps.maxBy(_._3)
      Console.err.println(s"newApp: $newApp")
      if (newApp._1 == action) s"MOVE $action" else s"MOVE $action ${newApp._1}"
    }
  }

  def calculateGiveCard(movesMap: Map[String, IndexedSeq[String]], cardsMap: Map[String, IndexedSeq[Card]], apps: List[Application]): String = {
    val gives = movesMap("GIVE").map(_.split(" ")(1).toInt)
    val head = cardsMap("HAND").head
    val newAppMap = apps.map(app => {
      val currentAppScore = app.applyCard(head)
      val priorityCalc = gives.map(give => {
        val newCard = head.applyTask((give, 0))
        (give, app.applyCard(newCard))
      })
      val newBestAppScore = priorityCalc.minBy(_._2)
      Console.err.println(s"App ${app.id} cur=$currentAppScore best=${newBestAppScore._2} after (${newBestAppScore._1})")
      (newBestAppScore._1, currentAppScore - newBestAppScore._2, newBestAppScore._2)
    }).groupBy(_._2)
    val minApps = newAppMap.minBy(_._1)._2
    val newApp = minApps.maxBy(_._3)
    Console.err.println(s"newApp: $newApp")
    s"GIVE ${newApp._1}"
  }

  def calculatePlayCard(movesMap: Map[String, IndexedSeq[String]], cardsMap: Map[String, IndexedSeq[Card]], apps: List[Application]): String = {                                            // FIXME
    var taskPrioritization = "RANDOM"
    def drawingNeeded = {
      cardsMap.contains("DRAW") && {
        val draws = cardsMap("DRAW").head
        draws.usefulCards.sum > 1 && draws.usefulCards.sum + draws.bonusCardsCount >= draws.technicalDebtCardsCount
      }
    }
    def refactoringNeeded = cardsMap("HAND").head.technicalDebtCardsCount > 0
    def taskPrioritizationNeeded = {
      val throwTake = movesMap("TASK_PRIORITIZATION").map(_.split(" ")).map(arr => (arr(1).toInt, arr(2).toInt)).filterNot(tt => tt._1 == tt._2)
      val head = cardsMap("HAND").head
      val newAppMap = apps.map(app => {
        val currentAppScore = app.applyCard(head)
        val priorityCalc = throwTake.map(tt => {
          val newCard = head.applyTask(tt)
          (tt._1, tt._2, app.applyCard(newCard))
        })
        val newBestAppScore = priorityCalc.minBy(_._3)
        //        Console.err.println(s"App ${app.id} cur=$currentAppScore best=${newBestAppScore._3} after (${newBestAppScore._1} ${newBestAppScore._2})")
        (newBestAppScore._1, newBestAppScore._2, currentAppScore - newBestAppScore._3, newBestAppScore._3)
      }).groupBy(_._3)
      val minApps = newAppMap.maxBy(_._1)._2
      val newApp = minApps.minBy(_._4)
      taskPrioritization = if (newApp._3 > 0) s"TASK_PRIORITIZATION ${newApp._1} ${newApp._2}" else "DUMMY"
      newApp._3 > 0
    }

    if (movesMap.contains("TRAINING") && drawingNeeded) {
      "TRAINING"
    } else if (movesMap.contains("CODING") && drawingNeeded) {
      "CODING"
    } else if (movesMap.contains("ARCHITECTURE_STUDY")) {
      "ARCHITECTURE_STUDY"
    } else if (movesMap.contains("DAILY_ROUTINE")) {
      "DAILY_ROUTINE"
    } else if (movesMap.contains("REFACTORING") && refactoringNeeded) {
      "REFACTORING"
    } else if (movesMap.contains("TASK_PRIORITIZATION") && taskPrioritizationNeeded) {
      taskPrioritization
    } else if (movesMap.contains("CODE_REVIEW")) {
      "CODE_REVIEW"
    } else "WAIT"
  }

  var count = 1

  // game loop
  while(true) {
    val gamePhase = readLine // can be MOVE, GIVE_CARD, THROW_CARD, PLAY_CARD or RELEASE
    Console.err.println(s"$gamePhase")
    val applicationsCount = readLine.toInt
    Console.err.println(s"$applicationsCount")

    applications = (for(i <- 0 until applicationsCount) yield {
      // trainingNeeded: number of TRAINING skills needed to release this application
      // codingNeeded: number of CODING skills needed to release this application
      // dailyRoutineNeeded: number of DAILY_ROUTINE skills needed to release this application
      // taskPrioritizationNeeded: number of TASK_PRIORITIZATION skills needed to release this application
      // architectureStudyNeeded: number of ARCHITECTURE_STUDY skills needed to release this application
      // continuousDeliveryNeeded: number of CONTINUOUS_DELIVERY skills needed to release this application
      // codeReviewNeeded: number of CODE_REVIEW skills needed to release this application
      // refactoringNeeded: number of REFACTORING skills needed to release this application
      val applicationData = readLine
      Console.err.println(s"$applicationData")

      val Array(objectType, _id, _trainingNeeded, _codingNeeded, _dailyRoutineNeeded, _taskPrioritizationNeeded, _architectureStudyNeeded, _continuousDeliveryNeeded, _codeReviewNeeded, _refactoringNeeded) = applicationData split " "

      val id = _id.toInt
      val trainingNeeded = _trainingNeeded.toInt
      val codingNeeded = _codingNeeded.toInt
      val dailyRoutineNeeded = _dailyRoutineNeeded.toInt
      val taskPrioritizationNeeded = _taskPrioritizationNeeded.toInt
      val architectureStudyNeeded = _architectureStudyNeeded.toInt
      val continuousDeliveryNeeded = _continuousDeliveryNeeded.toInt
      val codeReviewNeeded = _codeReviewNeeded.toInt
      val refactoringNeeded = _refactoringNeeded.toInt
      Application(id, trainingNeeded, codingNeeded, dailyRoutineNeeded, taskPrioritizationNeeded, architectureStudyNeeded, continuousDeliveryNeeded, codeReviewNeeded, refactoringNeeded)
    }).toList

    //    applications.foreach(app => Console.err.println(s"application ${app.id} has ${app.notEmptySkills.mkString(",")} not-empty skill"))

    for(i <- 0 until 2) {
      // playerLocation: id of the zone in which the player is located
      // playerPermanentDailyRoutineCards: number of DAILY_ROUTINE the player has played. It allows them to take cards from the adjacent zones
      // playerPermanentArchitectureStudyCards: number of ARCHITECTURE_STUDY the player has played. It allows them to draw more cards
      val playerData = readLine
      Console.err.println(s"$playerData")
      val Array(playerLocation, playerScore, playerPermanentDailyRoutineCards, playerPermanentArchitectureStudyCards) = (playerData split " ").filter(_ != "").map (_.toInt)
    }
    val cardLocationsCount = readLine.toInt
    Console.err.println(s"$cardLocationsCount")

    val cards = for(i <- 0 until cardLocationsCount) yield {
      // cardsLocation: the location of the card list. It can be HAND, DRAW, DISCARD or OPPONENT_CARDS (AUTOMATED and OPPONENT_AUTOMATED will appear in later leagues)
      val cardData = readLine
      Console.err.println(s"$cardData")
      val Array(cardsLocation, _trainingCardsCount, _codingCardsCount, _dailyRoutineCardsCount, _taskPrioritizationCardsCount, _architectureStudyCardsCount, _continuousDeliveryCardsCount, _codeReviewCardsCount, _refactoringCardsCount, _bonusCardsCount, _technicalDebtCardsCount) = cardData split " "
      val trainingCardsCount = _trainingCardsCount.toInt
      val codingCardsCount = _codingCardsCount.toInt
      val dailyRoutineCardsCount = _dailyRoutineCardsCount.toInt
      val taskPrioritizationCardsCount = _taskPrioritizationCardsCount.toInt
      val architectureStudyCardsCount = _architectureStudyCardsCount.toInt
      val continuousDeliveryCardsCount = _continuousDeliveryCardsCount.toInt
      val codeReviewCardsCount = _codeReviewCardsCount.toInt
      val refactoringCardsCount = _refactoringCardsCount.toInt
      val bonusCardsCount = _bonusCardsCount.toInt
      val technicalDebtCardsCount = _technicalDebtCardsCount.toInt
      Card(cardsLocation,
        trainingCardsCount,
        codingCardsCount,
        dailyRoutineCardsCount,
        taskPrioritizationCardsCount,
        architectureStudyCardsCount,
        continuousDeliveryCardsCount,
        codeReviewCardsCount,
        refactoringCardsCount,
        bonusCardsCount,
        technicalDebtCardsCount)
    }
    val cardsMap = cards.groupBy(_.cardsLocation)
    val possibleMovesCount = readLine.toInt
    Console.err.println(s"$possibleMovesCount")
    val moves = for(i <- 0 until possibleMovesCount) yield {
      val possibleMove = readLine
      Console.err.println(s"$possibleMove")
      possibleMove
    }
    val movesMap = moves.groupBy(str => str.split(" ")(0))

    val move = if (gamePhase.equals("MOVE")) {
      val possibleMoves = movesMap("MOVE")
      //      val movesIndex = possibleMoves.map(move => move.split(" ")(1).toInt).toList.sorted
      val movesIndex = possibleMoves.map(move => {
        val movesArr = move.split(" ")
        (movesArr(1).toInt, if (movesArr.length > 2) movesArr(2).toInt else movesArr(1).toInt )
      }).groupMap(_._1)(_._2)
      val closestMove = movesIndex.keys.find(_ > count).getOrElse(0)
      //      val possibleTakes = movesIndex(closestMove).map(_._2).distinct
      count = closestMove
      calculateCard(closestMove, movesIndex(closestMove), cardsMap, applications)
    } else if (gamePhase.equals("RELEASE")) {
      val possibleReleases = movesMap("RELEASE")
      val releasesIndex = possibleReleases.map(move => move.split(" ")(1).toInt).toList
      val releaseIndex = findRelease(cardsMap("HAND").toList.head, releasesIndex).map("RELEASE " + _).getOrElse("RANDOM")
      releaseIndex
    } else if (gamePhase.equals("PLAY_CARD")) {
      calculatePlayCard(movesMap, cardsMap, applications)
    } else if (gamePhase.equals("GIVE_CARD")) {
      calculateGiveCard(movesMap, cardsMap, applications)
    } else moves(0)

    // In the first league: RANDOM | MOVE <zoneId> | RELEASE <applicationId> | WAIT; In later leagues: | GIVE <cardType> | THROW <cardType> | TRAINING | CODING | DAILY_ROUTINE | TASK_PRIORITIZATION <cardTypeToThrow> <cardTypeToTake> | ARCHITECTURE_STUDY | CONTINUOUS_DELIVERY <cardTypeToAutomate> | CODE_REVIEW | REFACTORING;
    println(move)
  }
}
