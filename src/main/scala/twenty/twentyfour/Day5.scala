package twenty.twentyfour

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day5 extends App {

  val sampleInput = """47|53
                              |97|13
                              |97|61
                              |97|47
                              |75|29
                              |61|13
                              |75|53
                              |29|13
                              |97|29
                              |53|29
                              |61|53
                              |97|53
                              |61|29
                              |47|13
                              |75|47
                              |97|75
                              |47|61
                              |75|61
                              |47|29
                              |75|13
                              |53|13
                              |
                              |75,47,61,53,29
                              |97,61,53,29,13
                              |75,29,13
                              |75,97,47,61,53
                              |61,13,29
                              |97,13,75,29,47""".stripMargin

  val input: String = {
    val source = Source.fromFile("src/resources/input/24Day5.txt")
    try {
      source.getLines().mkString("\n")
    } finally {
      source.close()
    }
  }

  def parseInput(
      inputString: String
  ): (Map[Int, List[Int]], List[List[Int]]) = {
    val ruleRegex: Regex = """(\d+)\|(\d+)""".r
    val Array(rulesStr, updatesStr) = inputString.split("\n\n")
    val rules: Map[Int, List[Int]] =
      ruleRegex
        .findAllMatchIn(rulesStr)
        .map { m =>
          m.group(1).toInt -> m.group(2).toInt
        }
        .toList
        .groupMap(_._1)(_._2)
    val updates: List[List[Int]] =
      updatesStr.linesIterator.map(_.split(",").map(_.toInt).toList).toList
    (rules, updates)
  }

  val (rules, updates) = parseInput(input)

  def isValid(update: List[Int]): Boolean = {
    def rec(ls: List[Int], visited: List[Int]): Boolean = {
      ls match {
        case Nil => true
        case page :: pages =>
          !rules.getOrElse(page, List.empty).exists(visited.contains)
          && rec (pages, visited :+ page)
      }
    }
    rec(update, List.empty)
  }

  val res1 = updates.filter(isValid).map(x => x(x.size / 2)).sum

  println(res1)

  val notOrdered = updates.filterNot(isValid)

  @tailrec
  def fixOrder(order: List[Int], checkFromIndex: Int): List[Int] = {
    if (checkFromIndex == order.size) {
      return order
    }
    val visited = order.take(checkFromIndex)
    val (first, rest) =
      (order.take(checkFromIndex + 1), order.drop(checkFromIndex + 1))
    val ruleForCur = rules.getOrElse(order(checkFromIndex), List.empty)
    val ruleBreak = ruleForCur.exists(visited.contains)

    if (!ruleBreak) {
      fixOrder(order, checkFromIndex + 1)
    } else {
      val temp = visited.filter(ruleForCur.contains).head
      val newCheckFromIndex = first.indexOf(temp)
      val newOrder =
        (first.updated(newCheckFromIndex, first.last).init :+ temp) ++ rest
      fixOrder(newOrder, newCheckFromIndex)
    }

  }

  def fixedOrder = notOrdered.map(fixOrder(_, 0)).map(x => x(x.size / 2)).sum

  println(fixedOrder)

}
