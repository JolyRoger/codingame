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
     val filename = "resources/greencircle/1.txt"
     val bufferedSource = Source.fromFile(filename)
     val data = bufferedSource.getLines
     def readInt = if (data.hasNext) data.next.toInt else { System.exit(0); -1 }
     def readLine = if (data.hasNext) data.next else { System.exit(0); "" }
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
    private def diff(crs: (Int, Int)) = { val diff = crs._1 - crs._2; if (diff < 0) 0 else diff }
    def absentSkills(card: Card) = skills.zip(card.usefulCards).map(diff).sum
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
  }
  var applications = List.empty[Application]

  def findMoveApp(card: Card, moves: List[Int]) = {
    val sortedApps = applications.sortBy(app => app.absentSkills(card))
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
    val sortedApps = applications.sortBy(app => app.absentSkills(card))
    sortedApps.headOption.map(_.id)
  }

  var count = -1

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
      val movesIndex = possibleMoves.map(move => move.split(" ")(1).toInt).toList.sorted
      val closestMove = movesIndex.find(_ > count).getOrElse(0)
      count = closestMove
      "MOVE " + closestMove
    } else if (gamePhase.equals("RELEASE")) {
      val possibleReleases = movesMap("RELEASE")
      val releasesIndex = possibleReleases.map(move => move.split(" ")(1).toInt).toList
      val releaseIndex = findRelease(cardsMap("HAND").toList.head, releasesIndex).map("RELEASE " + _).getOrElse("RANDOM")
      releaseIndex
    } else moves(0)

    //    val move = if (gamePhase.equals("MOVE")) {
    //      val possibleMoves = movesMap("MOVE")
    //      val app = findMoveApp(cardsMap("HAND").toList.head, possibleMoves.toList.map(moveStr => moveStr.split(" ")(1).toInt))
    //      Console.err.println(s"Min app is ${app._2.map(_.id).get}")
    //      app._1.map(num => s"MOVE $num").getOrElse("RANDOM")
    //    } else "RANDOM"
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")


    // In the first league: RANDOM | MOVE <zoneId> | RELEASE <applicationId> | WAIT; In later leagues: | GIVE <cardType> | THROW <cardType> | TRAINING | CODING | DAILY_ROUTINE | TASK_PRIORITIZATION <cardTypeToThrow> <cardTypeToTake> | ARCHITECTURE_STUDY | CONTINUOUS_DELIVERY <cardTypeToAutomate> | CODE_REVIEW | REFACTORING;
    println(move)
  }
}
