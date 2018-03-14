package kmeans

import org.scalameter._

import scala.annotation.tailrec
import scala.collection.{GenSeq, _}
import scala.util.Random

class KMeans {

  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Point(x, y, z)
      }).to[mutable.ArrayBuffer]
  }

  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ => points(rand.nextInt(points.length))).to[mutable.ArrayBuffer]
  }

  def findClosest(p: Point, means: GenSeq[Point]): Point = {
    assert(means.nonEmpty)
    var minDistance = p.squareDistance(means.head)
    var closest = means.head
    var i = 1
    while (i < means.length) {
      val distance = p.squareDistance(means(i))
      if (distance < minDistance) {
        minDistance = distance
        closest = means(i)
      }
      i += 1
    }
    closest
  }

  //  [Test Description] 'kMeans' should work for
  // 'points' == GenSeq((0, 0, 1), (0,0, -1), (0,1,0), (0,10,0))
  // and
  // 'oldMeans' == GenSeq((0, -1, 0), (0, 2, 0))
  // and
  // 'eta' == 12.25
  //  [Observed Error] Util.equalPointSeq(KM.kMeans(points, means, eta), expected) was false KMeans(
  // Vector((0.0, 0.0, 1.0), (0.0, 0.0, -1.0), (0.0, 1.0, 0.0), (0.0, 10.0, 0.0)), means) should equal to
  // Vector((0.0, 0.0, 0.0), (0.0, 5.5, 0.0))
  //  [Lost Points] 4

  def classify(points: GenSeq[Point], means: GenSeq[Point]): GenMap[Point, GenSeq[Point]] = {
    val start = System.currentTimeMillis()
    // No speedup with par collections here!
    // WTF???
    val clustered = points.groupBy(p => findClosest(p, means))
    println(s"cluster time: ${System.currentTimeMillis() - start}")
    val empty = means.filterNot(clustered.keySet.contains(_)).map(m => m -> Nil).toMap
    clustered ++ empty
  }

  def findAverage(oldMean: Point, points: GenSeq[Point]): Point = if (points.isEmpty) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    points.seq.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / points.length, y / points.length, z / points.length)
  }

  def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point] = {
    oldMeans.map(m => findAverage(m, classified(m)))
  }

  def converged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean = {
    require(oldMeans.size == newMeans.size)
    oldMeans.zip(newMeans).forall(m => m._1.squareDistance(m._2) <= eta)
  }

  @tailrec
  final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point] = {
    val classified = classify(points, means)
    val newMeans = update(classified, means)
    if (!converged(eta)(means, newMeans)) kMeans(points, newMeans, eta) else newMeans // your implementation need to be tail recursive
  }
}

/** Describes one point in three-dimensional space.
  *
  * Note: deliberately uses reference equality.
  */
class Point(val x: Double, val y: Double, val z: Double) {
  private def square(v: Double): Double = v * v

  def squareDistance(that: Point): Double = {
    square(that.x - x) + square(that.y - y) + square(that.z - z)
  }



  private def round(v: Double): Double = (v * 100).toInt / 100.0

  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

  override def equals(other: Any): Boolean = other match {
    case that: Point =>
      (that canEqual this) &&
        x == that.x &&
        y == that.y &&
        z == that.z
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(x, y, z)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}


object KMeansRunner {

  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

  def main(args: Array[String]) {
        val kMeans = new KMeans()

        val numPoints = 500000
        val eta = 0.01
        val k = 32
        val points = kMeans.generatePoints(k, numPoints)
        val means = kMeans.initializeMeans(k, points)

        val seqtime = standardConfig measure {
          kMeans.kMeans(points, means, eta)
        }
        println(s"sequential time: $seqtime ms")

        val partime = standardConfig measure {
          val parPoints = points.par
          val parMeans = means.par
          kMeans.kMeans(parPoints, parMeans, eta)
        }
        println(s"parallel time: $partime ms")
        println(s"speedup: ${seqtime / partime}")

//    val lst = List(1,2,3,4,5)
//    println(lst.zip(lst.tail))

//    val res = kMeans.kMeans(GenSeq(new Point(0, 0, 1), new Point(0, 0, -1), new Point(0, 1, 0), new Point(0, 10, 0)), GenSeq(new Point(0, -1, 0), new Point(0, 2, 0)), 12.25)
//    assert(res == Vector((0.0, 0.0, 0.0), (0.0, 5.5, 0.0)))
  }

}
