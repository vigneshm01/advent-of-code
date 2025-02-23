package twenty.twentyfour

import java.time.LocalDateTime
import scala.io.Source

object Day4 extends App {
  type Grid = List[List[Char]]
  val input = {
    val source = Source.fromFile("src/resources/input/24Day4.txt")
    try {
      source
        .getLines()
        .toList
        .map(_.trim)
        .map(_.toList)
    } finally {
      source.close()
    }
  }
  val sampleInput =
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX""".stripMargin
      .split("\n")
      .map(_.trim)
      .map(_.toList)
      .toList

  case class Dir(dr: Int, dc: Int)

  val directions = List(
    Dir(dr = -1, dc = 0), //up
    Dir(dr = 1, dc = 0), //down
    Dir(dr = 0, dc = 1), //right
    Dir(dr = 0, dc = -1), //left
    Dir(dr = -1, dc = 1), //up-right
    Dir(dr = -1, dc = -1), //up-left
    Dir(dr = 1, dc = 1), //down-right
    Dir(dr = 1, dc = -1) //down-left
  )

  def boundCheck(x: Int, y: Int, grid: Grid): Boolean =
    x >= 0 && x < grid.size && y >= 0 && y < grid.head.size

  def scanner(x: Int, y: Int, dir: Dir, grid: Grid) = List.unfold((x, y)) {
    (x, y) =>
      Option.when(boundCheck(x, y, grid))(
        grid(x)(y) -> (x + dir.dr, y + dir.dc)
      )
  }

  def scanString(
                  target: String
                )(x: Int, y: Int, dir: Dir, grid: Grid): Boolean =
    scanner(x, y, dir, grid).take(target.length).corresponds(target)(_ == _)

  val scanXMAS = scanString("XMAS")

  def totalXMAS(grid: Grid): Int =
    List
      .tabulate(grid.size, grid.size) { (x, y) =>
        directions.count(dir => scanXMAS(x, y, dir, grid))
      }
      .flatten
      .sum

  println(LocalDateTime.now())
  val res1 = totalXMAS(sampleInput)
  println(s"Ans1: $res1")
  println(LocalDateTime.now())

  val UpRight = directions(4)
  val DownRight = directions(6)
  val DownLeft = directions(7)
  val UpLeft = directions(5)

  val xmasdirection =List(
    UpLeft -> DownRight,
    UpRight -> DownLeft,
    DownRight -> UpLeft,
    DownLeft -> UpRight
  )

  val scanMAS = scanString("MAS")

  def isMAS(x: Int, y: Int, grid: Grid): Boolean =
    grid(x)(y) match
      case 'A' =>
        val seen = xmasdirection.count { (transform, dir) =>
          scanMAS(x + transform.dr, y + transform.dc, dir, grid)
        }
        seen > 1
      case _ => false

  def totalMAS(grid: Grid): Int =
    Iterator
      .tabulate(grid.size, grid.size) { (x, y) =>
        if isMAS(x, y, grid) then 1 else 0
      }
      .flatten
      .sum

  val res2 = totalMAS(input)
  println(LocalDateTime.now())
  println(s"Ans1: $res2")
  println(LocalDateTime.now())

}
