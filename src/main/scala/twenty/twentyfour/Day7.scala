package twenty.twentyfour

import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.io.Source

object Day7 extends App{
  val sampleInput = """190: 10 19
                      |3267: 81 40 27
                      |83: 17 5
                      |156: 15 6
                      |7290: 6 8 6 15
                      |161011: 16 10 13
                      |192: 17 8 14
                      |21037: 9 7 18 13
                      |292: 11 6 16 20""".stripMargin.split("\n").toList.map(i => i.splitAt(i.indexOf(':'))).map(x => (x._1.toLong, x._2.trim.drop(2).split(" ").toList.map(_.toLong)))

  val input = {
    val source = Source.fromFile("src/resources/input/24Day7.txt")
    try {
      source
        .getLines()
        .toList
        .map(i => i.splitAt(i.indexOf(':')))
        .map{x =>
          (x._1.toLong, x._2.trim.drop(2).split(" ").toList.map(_.toLong))
        }
    } finally {
      source.close()
    }
  }

  println(input.size)


  def isCalibratable(req: Long, positions: List[Long]): Boolean = {
    positions match
      case first :: Nil =>
        first == req
      case first :: second :: rest =>
        isCalibratable(req, (first*second) :: rest) || isCalibratable(req, (first+second) :: rest)
  }

  def isCalibratableWithConcat(req: Long, positions: List[Long]): Boolean = {
    positions match
      case first :: Nil =>
        first == req
      case first :: second :: rest =>
        isCalibratableWithConcat(req, (first*second) :: rest) || isCalibratableWithConcat(req, (first+second) :: rest) || isCalibratableWithConcat(req, (first.toString ++ second.toString).toLong :: rest)
  }

  val samplePart1 = sampleInput.filter(isCalibratable).map(_._1).sum
  val actualPart1 = input.filter(isCalibratable).map(_._1).sum
  println(s"result for sample part1: ${samplePart1}")
  println(s"result for actual part1: ${actualPart1}")


  val samplePart2 = sampleInput.filter(isCalibratableWithConcat).map(_._1).sum
  val actualPart2 = input.filter(isCalibratableWithConcat).map(_._1).sum
  println(s"result for sample part2: ${samplePart2}")
  println(s"result for actual part2: ${actualPart2}")
}
