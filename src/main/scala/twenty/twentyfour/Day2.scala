package twenty.twentyfour

import scala.io.StdIn.readLine

object Day2 {
  def main(args: Array[String]): Unit = {
    var flag = true
    var reports: List[List[Int]] = List()

    while (flag) {
      val input = readLine()
      if (input == null || input.trim.isEmpty) {
        flag = false
      } else {
        val report = input.trim.split(" ").map(_.toInt).toList
        reports = reports :+ report
      }
    }


    def isSafe(values: List[Int]): Boolean = {
      val diff = values.sliding(2).map { case List(a, b) => b - a }.toList

      val allIncreasing = diff.forall(_ > 0)
      val allDecreasing = diff.forall(_ < 0)

      val res = diff.forall(x => math.abs(x) >= 1 && math.abs(x) <= 3) && (allIncreasing || allDecreasing)

      res
    }

    print("part 1 answer: \t")
    val safetyResult = reports.map(isSafe)
    val safetyScore = safetyResult.count(identity)
    println(safetyScore)


    def dampener(values: List[Int]): Boolean = {
      val diff = values.sliding(2).map { case List(a, b) => b - a }.toList

      println(diff)

      true
    }

    print("part 2 answer: \t")
    val unsafe = reports.zip(safetyResult).filter(!_._2).map(_._1)
    val dampenered = unsafe.map(dampener).count(identity)
    println(dampenered+safetyScore)

  }
}
