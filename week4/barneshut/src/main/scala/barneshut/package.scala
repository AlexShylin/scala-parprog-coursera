import barneshut.conctrees._
import common._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width: Float = maxX - minX

    def height: Float = maxY - minY

    def size: Float = math.max(width, height)

    def centerX: Float = minX + width / 2

    def centerY: Float = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX

    def massY: Float = centerY

    def mass: Float = 0

    def total: Int = 0

    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))
  }

  case class Fork(nw: Quad, ne: Quad, sw: Quad, se: Quad) extends Quad {
    val centerX: Float = nw.centerX + (nw.size / 2)
    val centerY: Float = nw.centerY + (nw.size / 2)
    val size: Float = nw.size * 2
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float = if (mass == 0) centerX else (nw.mass * nw.massX + ne.mass * ne.massX + sw.mass * sw.massX + se.mass * se.massX) / mass
    val massY: Float = if (mass == 0) centerY else (nw.mass * nw.massY + ne.mass * ne.massY + sw.mass * sw.massY + se.mass * se.massY) / mass
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      println(s"Fork $toString")
      println(b)
      // to the north west child
      if (b.inBoundsOf(nw)) Fork(nw.insert(b), ne, sw, se)

      // to the north east child
      else if (b.inBoundsOf(ne)) Fork(nw, ne.insert(b), sw, se)

      // to the south west child
      else if (b.inBoundsOf(sw)) Fork(nw, ne, sw.insert(b), se)

      // to the south east child
      else Fork(nw, ne, sw, se.insert(b))
    }

    def insert(bodies: Seq[Body]): Fork = bodies match {
      case Nil => this
      case head :: tail => this.insert(head).insert(tail)
    }



    override def toString: String = s"centerX: $centerX, centerY: $centerY, size: $size, mass: $mass, massX: $massX, massY: $massY, total: $total"
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body]) extends Quad {
    val mass: Float = bodies.map(_.mass).sum: Float
    val massX: Float = bodies.map(b => b.mass * b.x).sum / mass
    val massY: Float = bodies.map(b => b.mass * b.y).sum / mass
    val total: Int = bodies.size

    def insert(b: Body): Quad =
      if (size > minimumSize) {
        val halfSize = size / 2f
        val quarterSize = halfSize / 2f
        Fork(
          Empty(centerX - quarterSize, centerY - quarterSize, halfSize),
          Empty(centerX + quarterSize, centerY - quarterSize, halfSize),
          Empty(centerX - quarterSize, centerY + quarterSize, halfSize),
          Empty(centerX + quarterSize, centerY + quarterSize, halfSize)
        ).insert(bodies.+:(b))
      }
      else Leaf(centerX, centerY, size, bodies.+:(b))

    override def toString: String = s"centerX: $centerX, centerY: $centerY, size: $size, mass: $mass, massX: $massX, massY: $massY, total: $total"
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def inBoundsOf(quad: Quad): Boolean = {
      val halfSize = quad.size / 2f
      val leftBorder = quad.centerX - halfSize
      val rightBorder = quad.centerX + halfSize
      val topBorder = quad.centerY - halfSize
      val bottomBorder = quad.centerY + halfSize

      x >= leftBorder && x <= rightBorder && y >= topBorder && y <= bottomBorder
    }

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
        // no force
        case Leaf(_, _, _, bodies) =>
          bodies.foreach(b => addForce(b.mass, b.x, b.y))
        case Fork(nw, ne, sw, se) =>
          if (distance(x, y, quad.centerX, quad.centerY) > theta)
            addForce(quad.mass, quad.massX, quad.massY)
          else traverse(quad)
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

    override def toString: String = s"Body: {x: $x, y: $y}"
  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize: Float = boundaries.size / sectorPrecision
    val minSectorSize: Float = boundaries.height  / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- matrix.indices) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      def getIndex(f: (Boundaries, Body) => Float) = f(boundaries, b) * sectorPrecision

      val (indexX, indexY) = (
        getIndex(getPossibleX),
        getIndex(getPossibleY)
      )

      if (!matrix(indexY.toInt * sectorPrecision + indexX.toInt).toSet.contains(b))
        matrix(indexY.toInt * sectorPrecision + indexX.toInt) += b
      this
    }

    private def getPossibleX(boundaries: Boundaries, b: Body): Float = {
      val dif = boundaries.maxX - boundaries.minX

      val x = {
        if (b.x <= boundaries.minX)
          boundaries.minX
        else if (b.x >= boundaries.maxX)
          boundaries.maxX - boundaries.minX
        else b.x - boundaries.minX
      }
      x / dif
    }

    private def getPossibleY(boundaries: Boundaries, b: Body): Float = {
      val dif = boundaries.maxY - boundaries.minY

      val y = {
        if (b.y <= boundaries.minY)
          boundaries.minY
        else if (b.y >= boundaries.maxY)
          boundaries.maxY - boundaries.minY
        else b.y - boundaries.minY
      }
      y / dif
    }

    def apply(x: Int, y: Int): ConcBuffer[Body] = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      for {
        i <- matrix.indices
      } if (matrix(i) != that.matrix(i)) matrix(i) = merge(matrix(i), that.matrix(i))
      this
    }

    private def merge(concBuffer1: ConcBuffer[Body], concBuffer2: ConcBuffer[Body]): ConcBuffer[Body] = {
      concBuffer1.combine(concBuffer2)
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4

      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          val emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this (x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear(): Unit = timeMap.clear()

    def timed[T](title: String)(body: => T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        System.currentTimeMillis() - startTime
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: $totalTime ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString: String = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString "\n"
    }
  }

}
