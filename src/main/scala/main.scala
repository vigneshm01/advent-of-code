import scala.io.StdIn._

@main
def main(): Unit = {

  val x = readLine("Enter you name")
  println("Enter your age")
  val y = readInt()
  if (x == null) {
    print("yes null")
  }

  if (x.isEmpty){
    print("yes empty" +
      "")
  }
  println(s"Hello $x your age is $y")

}
