package twenty.twentyfour

import scala.io.Source

object Day1 {

  def main(args: Array[String]): Unit = {
//    var first :List[Int] = List()
//    var second :List[Int] = List()
//    var flag = true
//
//    while(flag){
//      val input = readLine()
//      if (input == null || input.trim.isEmpty){
//        flag = false
//      }
//      else{
//        val Array(a,b) = input.trim.split("   ")
//        first = first :+ a.toInt
//        second = second :+ b.toInt
//      }
//    }

    val (first, second) = {
      val source = Source.fromFile("src/resources/input/24Day1.txt")
      try {
        source.getLines()
          .map { line =>
            val Array(a, b) = line.trim.split("   ")
            (a.toInt, b.toInt)
          }
          .toList
          .unzip
      } finally {
        source.close()
      }
    }

    val list1 = first.sorted
    val list2 = second.sorted

    val listPair = list1.zip(list2)

    val result = listPair.map(x => (x._1 - x._2).abs)

    print("part1 Result:\t")
    println(result.sum)

    val freqInList2 = list2.groupBy(identity).map((k, v) => (k, v.size))

    val result2 = list1.map(x => x * freqInList2.getOrElse(x, 0))

    print("part1 Result:\t")
    println(result2.sum)

  }

}
