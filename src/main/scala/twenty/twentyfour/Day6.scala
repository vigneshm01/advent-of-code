package twenty.twentyfour

import scala.io.Source
import scala.collection.IndexedSeq._

object Day6 extends App{

  val sampleInput =
    """....#.....
      |.........#
      |..........
      |..#.......
      |.......#..
      |..........
      |.#..^.....
      |........#.
      |#.........
      |......#...
      |""".stripMargin.split("\n").map(_.trim).toList


  val input = {
    val source = Source.fromFile("src/resources/input/24Day6.txt")
    try {
      source
        .getLines()
        .toList
        .map(_.trim)
    } finally {
      source.close()
    }
  }

  type Coordinate = (Int, Int)
  extension (cord: Coordinate) infix def +(other: Coordinate) = (cord._1 + other._1 , cord._2 + other._2)

  enum Direction(vectorr: Coordinate):
    def vector: Coordinate = vectorr

    case North extends Direction(vectorr = (0, -1))
    case East extends Direction(vectorr = (1, 0))
    case South extends Direction(vectorr = (0, 1))
    case West extends Direction(vectorr = (-1, 0))
  end Direction

  object Direction:
    def fromChar(c: Char): Direction = c match
      case '^' => North
      case 'v' => South
      case '>' => East
      case '<' => West
  end Direction

  type PointOfView = (Coordinate, Direction)

  case class Lab(l: List[String], northSouthLength: Int, eastWestLength: Int):
    require(l.size == northSouthLength && l.forall(_.size == eastWestLength))

    def isWithinLab(x: Int, y: Int): Boolean = x >= 0 && x < eastWestLength && y >= 0 && y < northSouthLength

    def get(x: Int, y: Int): Char = {
      require(isWithinLab(x, y))
      l(y)(x)
    }

    def isObstacle(x: Int, y: Int): Boolean = isWithinLab(x, y) && get(x, y) == '#'

    def isObstacle(coord: Coordinate): Boolean = isObstacle(coord._1, coord._2)

    def replaceWith(x: Int, y: Int, c: Char): Lab =
      require(isWithinLab(x, y))
      Lab(l.updated(y, l(y).updated(x, c)), northSouthLength, eastWestLength)
  end Lab

  case class Guard(lab: Lab):
    def step(pov: PointOfView): PointOfView =
      val isLookingAtObstacle = lab.isObstacle(pov._1 + pov._2.vector)
      if isLookingAtObstacle then
        val newDirection = Guard.rotate(pov._2)
        (pov._1, newDirection)
      else
        (pov._1 + pov._2.vector, pov._2)

    def pathFrom(pov: PointOfView): LazyList[PointOfView] =
      val nextPov = step(pov)
      pov #:: pathFrom(nextPov)

    def simulateWithinLab(pov: PointOfView): LazyList[PointOfView] =
      pathFrom(pov).takeWhile((coord, _) => lab.isWithinLab(coord._1, coord._2))
  end Guard

  object Guard:
    def rotate(dir: Direction): Direction = dir match
      case Direction.North => Direction.East
      case Direction.East => Direction.South
      case Direction.South => Direction.West
      case Direction.West => Direction.North
  end Guard

  def countVisitedDistinctLocations(g: Guard, startingPov: PointOfView): Int =
    g.simulateWithinLab(startingPov).map(_._1).toSet.size


  def parse(l: List[String]): (Guard, PointOfView) =
    require(l.size > 0 && l.head.size > 0)
    val startingY = l.indexWhere(s => s.contains("^") || s.contains("<") || s.contains(">") || s.contains("v"))
    assert(startingY >= 0 && startingY < l.size)
    val startingX = l(startingY).indexWhere(c => c == '^' || c == '<' || c == '>' || c == 'v')
    assert(startingX >= 0 && startingX < l.head.size)
    val guardChar = l(startingY)(startingX)
    println(guardChar)
    val direction = Direction.fromChar(guardChar)
    val lab = Lab(l.map(s => s.replace(guardChar, '.')), northSouthLength = l.size, eastWestLength = l.head.size)
    val guard = Guard(lab)

    (guard, ((startingX, startingY), direction))


  val calculateOutput: List[String] => Int = parse andThen countVisitedDistinctLocations

  println(s"result for sample part1: ${calculateOutput(sampleInput)}")
  println(s"result for actual part1: ${calculateOutput(input)}")

  def looping(guard: Guard, startingPov: PointOfView): Boolean =
    val followedPath = guard.simulateWithinLab(startingPov).take(guard.lab.eastWestLength * guard.lab.northSouthLength + 1)
    followedPath.size >= guard.lab.eastWestLength * guard.lab.northSouthLength

  def possibleObstaclesPositionsNumber(g: Guard, startingPov: PointOfView): Int =
    val possibleObstaclesPositions =
      (
        for
          x <- 0 to g.lab.eastWestLength
          y <- 0 to g.lab.northSouthLength
          if g.lab.isWithinLab(x, y) && (x, y) != startingPov._1
        yield (x, y)
        )

    val newPossibleGuards =
      possibleObstaclesPositions.map(obstaclePos =>
        val newLab = g.lab.replaceWith(obstaclePos._1, obstaclePos._2, '#')
        Guard(newLab)
      )
    newPossibleGuards.count(g => looping(g, startingPov))

  val calOutput2 = parse andThen possibleObstaclesPositionsNumber
  println(s"result for sample part2: ${calOutput2(sampleInput)}")
  println(s"result for actual part2: ${calOutput2(input)}")
}
