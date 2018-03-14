
object Test {

  def scanLeft[A](input: Array[A], a0: A, f: (A, A) => A, output: Array[A]): Unit = {

    println(output.map(_ => f(input.reduce(f), a0)))
  }

  def main(args: Array[String]): Unit = {
    val input = Array[Int](1, 3, 8)
    val output = Array.fill(4){0}
    scanLeft[Int](input, 100, _ + _, output)
    println(output.mkString(" "))
  }

}
