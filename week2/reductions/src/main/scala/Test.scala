
object Test extends App {
  def max(xs: Array[Int]): Int = {
    xs.fold[Int](Int.MinValue)((x1, x2) => if (x1 > x2) x1 else x2)
  }

  println(max(Array(1, 5, 3, 8, 4, 90, 5, 2, 45, 6, 7, 4)))
  println("--")
  println(Array('e', '2', 'f', '1').par.aggregate(0)((count, c) => if (c.isDigit) count + 1 else count, _ + _))
  println("--")
}
