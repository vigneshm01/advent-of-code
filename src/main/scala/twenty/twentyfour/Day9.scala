package twenty.twentyfour

import scala.annotation.tailrec
import scala.io.Source

object Day9 extends App {
  val sampleInput = "2333133121414131402"
  val input = {
    val source = Source.fromFile("src/resources/input/24Day9.txt")
    try {
      source.getLines().next()
    } finally {
      source.close()
    }
  }


  def convertToId(diskmap: List[String]): List[String] = {
    val out = diskmap.foldLeft((List.empty[String], 0, true)) {
      case ((res, id, isFile), elem) =>
        if (isFile) {
          val upres = res ++ List.fill(elem.toInt)(id.toString)
          (upres, id + 1, false)
        } else {
          val upres = res ++ List.fill(elem.toInt)(".")
          (upres, id, true)
        }
    }
    println(out._1)
    out._1
  }

  def compress(diskIdmap: List[String]): List[String] = {
    @tailrec
    def eliminate(disk: List[String], acc: List[String]): List[String] = {
      if (disk.isEmpty) {
        acc
      } else if (disk.head != ".") {
        eliminate(disk.tail, acc :+ disk.head)
      }
      else {
        eliminate(disk.last +: disk.tail.init, acc)
      }
    }

    val x = eliminate(diskIdmap, List.empty)
    println(x)
    x
  }

  def checksum(disk: List[String]): Long = {
    disk.zipWithIndex.map(x => x._1.toLong * x._2).sum
  }

  val output1 = convertToId andThen compress andThen checksum
  println(s"result for sample part1: ${output1(sampleInput.toList.map(_.toString))}")
  println(s"result for  part1: ${output1(input.toList.map(_.toString))}")



}
