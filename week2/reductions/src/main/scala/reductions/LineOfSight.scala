package reductions

import common.parallel
import org.scalameter._

object LineOfSightRunner {

  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val outputSeq = new Array[Float](length + 1)
    val outputPar = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, outputSeq)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, outputPar, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
    if (outputPar sameElements outputSeq) {
      println("parallel result == sequential result")
    } else {
      println("parallel result != sequential result")
    }
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    if (input.isEmpty) Unit
    else {
      for (i <- 1 until input.length) {
        val res = input(i) / i
        val prev = output(i - 1)
        output(i) = max(res, prev)
      }
    }
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious: Float = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
    */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    def upsweepSequentialInner(from: Int, accMaxAngleTan: Float): Float = {
      if (from == until) accMaxAngleTan
      else {
        val currMaxAngleTan = input(from) / from
        upsweepSequentialInner(from + 1, max(accMaxAngleTan, currMaxAngleTan))
      }
    }

    upsweepSequentialInner(from, 0)
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
    * returns the reduction tree for that part of the array.
    *
    * The reduction tree is a `Leaf` if the length of the specified part of the
    * array is smaller or equal to `threshold`, and a `Node` otherwise.
    * If the specified part of the array is longer than `threshold`, then the
    * work is divided and done recursively in parallel.
    */
  def upsweep(input: Array[Float], from: Int, end: Int,
              threshold: Int): Tree = {
    if (end - from <= threshold) Leaf(from, end, upsweepSequential(input, from, end))
    else {
      val mid = from + ((end - from) / 2)
      val (left, right) = parallel(
        upsweep(input, from, mid, threshold),
        upsweep(input, mid, end, threshold))
      Node(left, right)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
    * `until`, and computes the maximum angle for each entry of the output array,
    * given the `startingAngle`.
    */
  def downsweepSequential(input: Array[Float], output: Array[Float],
                          startingAngle: Float, from: Int, until: Int): Unit = {
    def downsweepSequentialInner(from: Int): Unit = {
      if (from == until) Unit
      else {
        val currMaxAngleTan = input(from) / from
        if (from > 0) output(from) = max(output(from - 1), currMaxAngleTan)
        else output(from) = max(output(from), currMaxAngleTan)
        downsweepSequentialInner(from + 1)
      }
    }

    output(from) = startingAngle
    downsweepSequentialInner(from)
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
    * reduction `tree` in parallel, and then calls `downsweepTraverse` to write
    * the `output` angles.
    */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
                tree: Tree): Unit = {
    tree match {
      case Node(left, right) => parallel(
        downsweep(input, output, startingAngle, left),
        downsweep(input, output, startingAngle, right))
      case Leaf(from, until, maxPrevious) =>
        downsweepSequential(input, output, maxPrevious, from, until)
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
                     threshold: Int): Unit = {
    val tree = upsweep(input, 0, input.length, threshold)
    downsweep(input, output, 0, tree)
    output(0) = 0f
  }
}
