package twenty.twentyfour

import scala.io.Source

object Day8 extends App {
  val sampleInput1 =
    """............
      |........0...
      |.....0......
      |.......0....
      |....0.......
      |......A.....
      |............
      |............
      |........A...
      |.........A..
      |............
      |............""".stripMargin.split("\n").map(_.trim).toList

  val input = {
    val source = Source.fromFile("src/resources/input/24Day8.txt")
    try {
      source
        .getLines()
        .toList
        .map(_.trim)
    } finally {
      source.close()
    }
  }

  case class AntennaMap(
      width: Int,
      height: Int,
      antennaGroups: Iterable[Seq[Location]]
  )

  case class Location(x: Int, y: Int) {
    def -(other: Location): Vec = Vec(x - other.x, y - other.y)

    def +(vec: Vec): Location = Location(x + vec.dx, y + vec.dy)

    def within(map: AntennaMap): Boolean =
      x >= 0 && x < map.width && y >= 0 && y < map.height
  }

  case class Vec(dx: Int, dy: Int)

  def parse(lines: List[String]): AntennaMap = {
    val antennae: Seq[(Char, Location)] = for {
      (line, y) <- lines.zipWithIndex
      (char, x) <- line.zipWithIndex
      if Character.isLetterOrDigit(char) || char == '#'
    } yield char -> Location(x, y)

    AntennaMap(
      lines.head.length,
      lines.size,
      antennae.groupMap(_._1)(_._2).values
    )
  }

  def countAntiNodes(antennaMap: AntennaMap): Int = {
    val antinodes = for {
      antennaGroup <- antennaMap.antennaGroups
      Seq(a, b) <- antennaGroup.combinations(2)
      antinode <- Seq(a + (a - b), b + (b - a))
      if antinode within antennaMap
    } yield antinode
    antinodes.toSet.size
  }

  println(parse(sampleInput1))

  val output1 = parse andThen countAntiNodes

  println(s"result for sample part1: ${output1(sampleInput1)}")
  println(s"result for actual part1: ${output1(input)}")

  def countAntiNodesPart2(antennaMap: AntennaMap): Int = {
    val antinodes = for {
      antennaGroup <- antennaMap.antennaGroups
      Seq(a, b) <- antennaGroup.combinations(2)
      antinode <- Iterator.iterate(a)(_ + (a-b)).takeWhile(_.within(antennaMap)) ++
        Iterator.iterate(b)(_ + (b-a)).takeWhile(_.within(antennaMap))
    } yield antinode
    antinodes.toSet.size
  }

  val output2 = parse andThen countAntiNodesPart2

  println(s"result for sample part1: ${output2(sampleInput1)}")
  println(s"result for actual part1: ${output2(input)}")
}
