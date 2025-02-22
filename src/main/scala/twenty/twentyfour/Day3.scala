package twenty.twentyfour

import scala.io.Source

object Day3 extends App {
  val input = {
    val source = Source.fromFile("src/resources/input/24Day3.txt")
    try {
      source
        .getLines
        .mkString("")
    }
    finally {
      source.close()
    }
  }

  val sampleInput =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  val mulPattern = "mul\\((\\d{1,3})\\,(\\d{1,3})\\)".r
  //regex of mull(12,23) is mul\(\d{1,3}\,\d{1,3}\) we add grouping by having another additon bracket mul\((\d{1,3})\,(\d{1,3})\)
  val mul = mulPattern.findAllMatchIn(input).map(m => (m.group(1).toInt, m.group(2).toInt)).toList

//  val string: List[String] = regex.findAllIn(source).toList

  val res = mul.map(x => x._1 * x._2).sum
  println("********************************************")
  println("*********** PART 1 ANSWER ******************")
  println(s"******  $res   *******")
  println("********************************************")

  val sampleInput2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  val regex2 = "mul\\(\\d{1,3}\\,\\d{1,3}\\)|do\\(\\)|don't\\(\\)".r

  val (_, ans2) = regex2.findAllIn(input).foldLeft((true, 0)){
    case ((true, sum), mulPattern(a,b))  => (true, sum + (a.toInt * b.toInt))
    case ((_, sum), "don't()") => (false, sum)
    case ((_, sum), "do()") => (true, sum)
    case ((false, sum), _) => (false,sum)

  }

  println("********************************************")
  println("*********** PART 2 ANSWER ******************")
  println(s"******  $ans2   *******")
  println("********************************************")


}
