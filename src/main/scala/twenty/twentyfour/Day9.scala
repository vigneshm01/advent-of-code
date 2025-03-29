package twenty.twentyfour

import java.time.{Duration, LocalDateTime}
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

    eliminate(diskIdmap, List.empty)
  }

  def checksum(disk: List[String]): Long = {
    disk.zipWithIndex.map(x => x._1.toLong * x._2).sum
  }

  val output1 = convertToId andThen compress andThen checksum
  println(s"result for sample part1: ${output1(sampleInput.toList.map(_.toString))}")
  val strtTime = LocalDateTime.now()
  println(s"result for  part1: ${output1(input.toList.map(_.toString))}")
  println(s"It took ${Duration.between(strtTime, LocalDateTime.now()).toSeconds} seconds")


  def part2(input: String): Long = {
    enum Block(val size: Int):
      case Free(s: Int) extends Block(s)
      case File(s: Int, i: Int) extends Block(s)

      def index = this match
        case Free(size) => None
        case File(size, id) => Some(id)

      def canInsert(block: Block) = this match
        case Free(size) => size >= block.size
        case _ => false

    extension (free: Block.Free)
      def insert(b: Block): Seq[Block] =
        if (b.size < free.size) {
          Seq(b, Block.Free(free.size-b.size))
        } else {
          Seq(b)
        }

    type Disk = Seq[Block]
    extension (disk: Disk)
      def checksum: Long = disk
        .flatMap(b => Vector.fill(b.size)(b.index.getOrElse(0)))
        .zipWithIndex
        .map(_.toLong * _)
        .sum

    def createDisk(input: String): Disk = {
      val intInput = input.toList.map(_ - '0')
      val fileGroups = intInput.grouped(2).toVector
      val zipped = fileGroups.zipWithIndex
      val disk = zipped.flatMap{
        case (List(file, free), idx) =>
          Vector(Block.File(file, idx), Block.Free(free))
        case (List(file), idx) =>
          Vector(Block.File(file, idx))
        case _ => Nil
      }
      disk
    }

    def compact(disk: Disk): Disk = {
      @tailrec
      def recCompact(disk: Disk, acc: Disk): Disk =
        disk.lastOption match
          case None =>
            acc
          case Some(last@Block.Free(_)) =>
            recCompact(disk.init, last +: acc)
          case Some(last@Block.File(size, _)) =>
            val fitter = disk
              .zipWithIndex
              .find((block, _) => block.canInsert(last))

            fitter match
              case None =>
                recCompact(disk.init, last +: acc)
              case Some(free@Block.Free(_), id) =>
                val newDisk = disk.take(id) ++ free.insert(last) ++ disk.drop(id + 1).init
                recCompact(newDisk, Block.Free(last.size) +: acc)


      recCompact(disk, Vector.empty)
    }
    val disk = createDisk(input)
    compact(disk).checksum
  }


  println(s"result for sample part2: ${part2(sampleInput)}")
  val strtTime2 = LocalDateTime.now()
  println(s"result for  part1: ${part2(input)}")
  println(s"It took ${Duration.between(strtTime2, LocalDateTime.now()).toSeconds} seconds")
}
