package twenty.twentyfour

import scala.annotation.tailrec

object Day11 extends App {
  val sampleInput: Vector[Long] = Vector(125, 17)
  val input: Vector[Long] = Vector(4610211, 4, 0, 59, 3907, 201586, 929, 33750)

  def change(stone: Long): Vector[Long] = {
    if stone == 0
    then Vector(1)
    else if stone.toString.length % 2 == 0
    then
      val stoneStr = stone.toString
      val (left, right) = stoneStr.splitAt(stoneStr.length / 2)
      Vector(left.toLong, right.toLong)
    else
      Vector(stone * 2024)
  }

  def countStones(stones: Vector[Long], blinks: Int): Long = {
    @tailrec
    def blink(arrangement: Vector[Long], iteration: Int): Vector[Long] = {
      if iteration > blinks
      then arrangement
      else
        val newArrangement = arrangement.flatMap(change)
        blink(newArrangement, iteration + 1)
    }

    blink(stones, 1).size
  }


  println(s"result for part1 sample Input: ${countStones(sampleInput, 25)}")
  println(s"result for part1 actual Input: ${countStones(input, 25)}")


  def countStonesOrdered(stones: Vector[Long], blinks: Int): Long = {
    type Stones = Map[Long, Long]
    extension(stones: Stones)
      def update(stone: Long, change: Long): Stones = {
        stones.updatedWith(stone){
          case None => Some(change)
          case Some(n) =>
            val count = n + change
            if count == 0 then None else Some(count)
        }
      }

    val stonesOrder: Stones = stones.groupBy(identity).map((k,v) => (k, v.size.toLong))

    def blink(stonesToChange: Stones): Stones = stonesToChange.foldLeft(stonesToChange){
      case (accStonesOrder, (k,v)) =>
        change(k) match
          case Vector(1) => accStonesOrder.update(k, -v).update(1, v)
          case Vector(a, b) => accStonesOrder.update(k, -v).update(a, v).update(b, v)
          case Vector(x) => accStonesOrder.update(k, -v).update(x,v)
          case _ => throw MatchError("this is expected")
    }

    Iterator
      .iterate(stonesOrder)(blink)
      .drop(blinks)
      .next()
      .values
      .sum

  }

  println(s"result for part2 sample Input: ${countStonesOrdered(sampleInput, 75)}")
  println(s"result for part2 actual Input: ${countStonesOrdered(input, 75)}")

}
