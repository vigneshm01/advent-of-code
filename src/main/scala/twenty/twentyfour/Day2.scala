package twenty.twentyfour

import scala.io.Source

object Day2 {
  def main(args: Array[String]): Unit = {

    val reports = {
      val source = Source.fromFile("src/resources/input/24Day2.txt")
      try {
        source
          .getLines()
          .map { line => line.trim.split(" ").map(_.toInt).toList }
          .toList
      } finally {
        source.close()
      }
    }

    def isSafe(values: List[Int]): Boolean = {
      val diff = values.sliding(2).map { case List(a, b) => b - a }.toList

      val allIncreasing = diff.forall(_ > 0)
      val allDecreasing = diff.forall(_ < 0)

      val res = diff.forall(x =>
        math.abs(x) >= 1 && math.abs(x) <= 3
      ) && (allIncreasing || allDecreasing)

      res
    }

    print("part 1 answer: \t")
    val safetyResult = reports.map(isSafe)
    val safetyScore = safetyResult.count(identity)
    println(safetyScore)

    def dampener(l: List[Int]): Boolean = 
      (0 until l.size).map(n => l.take(n) ++ l.drop(n+1)).toList.map(isSafe).exists(identity)

    /**
     * better approach may be is to produce boolean list to identify culprit value (1,2,3,7,4)
     * like (true, true, true, false, true) and then check `isSafe` by remove value at index 3
     */

    print("part 2 answer: \t")
    val unsafe = reports.zip(safetyResult).filter(!_._2).map(_._1)
    val dampenered = unsafe.map(dampener).count(identity)
    println(dampenered)
    println(dampenered + safetyScore)

  }

}
