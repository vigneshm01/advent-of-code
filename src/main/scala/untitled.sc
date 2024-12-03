def findFirstInvalidDiffIndex(nums: List[Int]): Int = {
  nums.indexWhere { x =>
    !(math.abs(x) >= 1 && math.abs(x) <= 3)
  }
}

val testCases = List(
  List(1, 5, 1, 1),
  List(-2, -1, -4, -1),
  List(2, -1, 2, 1),
  List(-2, -2, 0, -3)
)

testCases.foreach { list =>
  println(s"$list: ${findFirstInvalidDiffIndex(list)}")
}