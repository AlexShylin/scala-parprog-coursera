package reductions

import common._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

  def main(args: Array[String]): Unit = {
    val length = 1000
    val chars = Array.fill(length / 2) {
      '('
    } ++ Array.fill((length / 2) + 1) {
      ')'
    }
    val threshold = 100
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
    if (parResult == seqResult) {
      println("parallel result == sequential result")
    } else {
      println("parallel result != sequential result")
    }
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    var openParenthesesCounter = 0
    for (p <- chars if openParenthesesCounter >= 0) p match {
      case '(' => openParenthesesCounter += 1
      case ')' => openParenthesesCounter -= 1
      case _ =>
    }
    openParenthesesCounter == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int): Int = {
      var openParenthesesCounter = 0
      var i = idx
      while (i < until) {
        chars(i) match {
          case '(' => openParenthesesCounter += 1
          case ')' => openParenthesesCounter -= 1
          case _ =>
        }
        i += 1
      }
      openParenthesesCounter
    }

    def reduce(from: Int, until: Int): Int = {
      if (until - from == threshold) traverse(from, until)
      else {
        val next = {
          val v = from + threshold
          if (v > until) until else v
        }
        val (res1, res2) = parallel(reduce(from, next), reduce(next, until))
        res1 + res2
      }
    }

    if (chars.head == ')') false
    else reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
