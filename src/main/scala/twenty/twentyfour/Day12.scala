package twenty.twentyfour

import scala.io.Source
import scala.collection.mutable as mut

object Day12 extends App {
  val sampleInput = """RRRRIICCFF
                      |RRRRIICCCF
                      |VVRRRCCFFF
                      |VVRCCCJFFF
                      |VVVVCJJCFE
                      |VVIVCCJJEE
                      |VVIIICJJEE
                      |MIIIIIJJEE
                      |MIIISIJEEE
                      |MMMISSJEEE""".stripMargin.split("\n").toVector.map(_.trim)

  val input = {
    val source = Source.fromFile("src/resources/input/24Day12.txt")
    try {
      source
        .getLines()
        .toVector.map(_.trim)
    } finally {
      source.close()
    }
  }

  type Position = (Int, Int)
  type Region = Vector[Position]

  def cardinalPositions(p: Position): List[Position] = {
    List((p._1 - 1, p._2), (p._1 + 1, p._2), (p._1, p._2 - 1), (p._1, p._2 + 1))
  }

  case class PlantMap(plants: Vector[String]) {
    def length: Int = plants.head.length

    def width: Int = plants.size

    def indices: Vector[Position] = {
      val idx = for {
        i <- 0 until length
        j <- 0 until width
      } yield (i, j)
      idx.toVector
    }

    def apply(p: Position): Char = plants(p._1)(p._2)

    def isDefined(pos: Position): Boolean = pos._1 >= 0 && pos._1 < width && pos._2 >= 0 && pos._2 < length

    def get(pos: Position): Option[Char] = Option.when(isDefined(pos))(apply(pos))

    def floodFill(pos: Position): Region = {
      val queue = mut.Queue[Position]()
      val char = apply(pos)
      val visited = mut.Set[Position]()
      queue.addOne(pos)
      while (queue.nonEmpty) {
        val pos = queue.removeHead()
        if (get(pos).contains(char) && !visited.contains(pos)) {
          queue.addAll(cardinalPositions(pos))
          visited.add(pos)
        }
      }
      visited.toVector
    }

    def regions: List[Region] = {
      List.unfold[Vector[Position], Vector[Position]](this.indices) { idx =>
        idx.headOption.map { pos =>
          val matchingPos = floodFill(pos)
          (matchingPos, idx.diff(matchingPos))
        }
      }
    }

    def neighborPositions(pos: Position): List[Position] = {
      (pos._1 - 1 to pos._1 + 1).flatMap { x =>
        (pos._2 - 1 to pos._2 + 1).flatMap { y =>
          Option.when(x != pos._1 || y != pos._2)((x, y))
        }
      }.toList
    }


    def optionalCardinalPosition(pos: Position): List[Option[Char]] =
      cardinalPositions(pos).map(get)

    def optionalNeighbors(pos: Position): List[Option[Char]] = {
      neighborPositions(pos).map(get)
    }
  }


  extension (region: Region) {
    def asPlantMap: PlantMap = {
      val length = region.maxBy(_._1)._1 + 1
      val width = region.maxBy(_._2)._2 + 1
      val dotGrid = Array.fill(length, width)('.')
      for ((x, y) <- region if x >= 0 && x < length && y >= 0 && y < width) {
        dotGrid(x)(y) = '#'
      }
      val x = dotGrid.map(_.mkString).toVector
      PlantMap(x)
    }

    def inflate: Region = {
      region.flatMap((x, y) => List((x * 2, y * 2), (x * 2 + 1, y * 2), (x * 2, y * 2 + 1), (x * 2 + 1, y * 2 + 1)))
    }

    def area: Int = region.size
    def perimeter: Int = {
      val regionMap = region.asPlantMap
      region.map(p => regionMap.optionalCardinalPosition(p).count(_.forall(_ != '#'))).sum
    }

    def side: Int = {
      val bigRegion = region.inflate
      val regionMap = bigRegion.asPlantMap
      bigRegion.count { (x, y) =>
        val neighborCount = regionMap.optionalNeighbors(x, y).count(_.contains('#'))
        neighborCount match {
          case 3 | 4 | 7 => true
          case _ => false
        }
      }
    }
  }

  def part1(in: Vector[String]): Int  = PlantMap(in).regions.map(r => r.area * r.perimeter).sum

  def part2(in: Vector[String]): Int = PlantMap(in).regions.map(r => r.area * r.side).sum

  println(s"part1 sample input result: ${part1(sampleInput)}")
  println(s"part1 actual input result: ${part1(input)}")

  println(s"part2 sample input result: ${part2(sampleInput)}")
  println(s"part2 actual input result: ${part2(input)}")
}
