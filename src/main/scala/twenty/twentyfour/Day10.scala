package twenty.twentyfour

import scala.io.Source

object Day10 extends App {
  val sampleInput =
    """89010123
      |78121874
      |87430965
      |96549874
      |45678903
      |32019012
      |01329801
      |10456732
      |""".stripMargin.split("\n").toVector.map(_.trim.toVector.map(_ - '0'))

  val input =  {
    val source = Source.fromFile("src/resources/input/24Day10.txt")
    try {
      source
        .getLines()
        .toVector
        .map(_.trim.toVector.map(_ - '0'))
    } finally {
      source.close()
    }
  }

  type Pos = (Int, Int)
  extension(pos: Pos)
    def +(other: Pos): Pos = {
      (pos(0) + other(0), pos(1)+other(1))
    }

  type TopoMap = Vector[Vector[Int]]
  extension(topoMap: TopoMap)
    def apply(pos: Pos): Int = topoMap(pos(0))(pos(1))
    def inBounds(pos: Pos): Boolean = pos(0) >= 0 && pos(0) < topoMap.size && pos(1)>=0 && pos(1) < topoMap.head.size
    def positions: Seq[Pos] =
      for
        row <- topoMap.indices
        col <- topoMap.head.indices
      yield (row, col)

  type Graph = Map[Pos, Set[Pos]]

  def computeGraph(topoMap: TopoMap): Graph = {
    def reachables(pos: Pos): Set[Pos] = {
      val movements: Set[Pos] = Set((0, 1), (0, -1), (1, 0), (-1, 0))
      movements
        .map(offset => pos + offset)
        .filter(movedPos => topoMap.inBounds(movedPos) && topoMap(movedPos) == topoMap(pos)+1)
    }

    topoMap.positions.map(pos => pos -> reachables(pos)).toMap

  }

  def part1(topoMap: TopoMap): Int = {
    val graph = computeGraph(topoMap)
    def reachables(pos: Pos): Set[Pos] =
      if topoMap(pos) == 9
        then Set(pos)
      else graph(pos).flatMap(reachables)

    topoMap
      .positions
      .filter(pos => topoMap(pos) == 0)
      .map(pos => reachables(pos).size)
      .sum
  }

  println(s"part1 sample input result: ${part1(sampleInput)}")
  println(s"part1 actual input result: ${part1(input)}")

  def part2(topo: TopoMap): Int =
    val graph = computeGraph(topo)

    def routes(pos: Pos): Int =
      if topo(pos) == 9
      then 1
      else graph(pos).toSeq.map(routes).sum

    topo.positions
      .filter(pos => topo(pos) == 0)
      .map(routes)
      .sum

  println(s"part2 sample input result: ${part2(sampleInput)}")
  println(s"part2 actual input result: ${part2(input)}")
}
