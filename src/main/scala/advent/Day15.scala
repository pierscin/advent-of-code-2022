package advent

import scala.collection.mutable

object Day15 extends App {

  val testInput = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
                    |Sensor at x=9, y=16: closest beacon is at x=10, y=16
                    |Sensor at x=13, y=2: closest beacon is at x=15, y=3
                    |Sensor at x=12, y=14: closest beacon is at x=10, y=16
                    |Sensor at x=10, y=20: closest beacon is at x=10, y=16
                    |Sensor at x=14, y=17: closest beacon is at x=10, y=16
                    |Sensor at x=8, y=7: closest beacon is at x=2, y=10
                    |Sensor at x=2, y=0: closest beacon is at x=2, y=10
                    |Sensor at x=0, y=11: closest beacon is at x=2, y=10
                    |Sensor at x=20, y=14: closest beacon is at x=25, y=17
                    |Sensor at x=17, y=20: closest beacon is at x=21, y=22
                    |Sensor at x=16, y=7: closest beacon is at x=15, y=3
                    |Sensor at x=14, y=3: closest beacon is at x=15, y=3
                    |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin
    .split("\n")

  def parseLine(s: String) = {
    val indices = mutable.ArrayBuffer.empty[Int]

    s.zipWithIndex.map { case (c, i) => if (c == '=') indices += (i + 1) }

    val sensor = (
      s.substring(indices(0)).takeWhile(_ != ',').toInt,
      s.substring(indices(1)).takeWhile(_ != ':').toInt
    )
    val beacon = (
      s.substring(indices(2)).takeWhile(_ != ',').toInt,
      s.substring(indices(3)).toInt
    )

    List(sensor, beacon)
  }

  def diffTuples(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    (a._1 - b._1, a._2 - b._2)
  }

  def md(a: (Int, Int), b: (Int, Int)) = {
    val diff = diffTuples(a, b)

    diff._1.abs + diff._2.abs
  }

  def part1(input: Seq[String], row: Int) = {
    val beacons = mutable.ArrayBuffer.empty[(Int, Int)]
    val sensors = mutable.ArrayBuffer.empty[(Int, Int)]

    input.map(parseLine).map { case sensor :: beacon :: Nil =>
      beacons += beacon
      sensors += sensor
    }

    val all = (beacons.toSet ++ sensors.toSet)

    val res = mutable.Set.empty[Int]

    (beacons zip sensors).foreach { case (b, s) =>
      val d = md(s, b)

      if (-d <= row - s._2 && d >= row - s._2) {
        val radius = d - (row - s._2).abs
        val y = row

        for {
          r <- -radius to radius
          x = s._1 + r
        } {
          if (!all.contains((x, y))) {
            res += x
          }
        }
      }
    }

    res.size
  }

  def part2(input: Seq[String], limit: Int): Long = {
    val sensorToRadius = input
      .map(parseLine)
      .map { case sensor :: beacon :: Nil =>
        (sensor._1, sensor._2) -> md(sensor, beacon)
      }
      .toMap

    val sensors = sensorToRadius.keys

    val (aCoeffs, bCoeffs) = coefficientsOfLinesOutsideSensorRadius(
      sensorToRadius
    )

    for {
      a <- aCoeffs
      b <- bCoeffs
    } {
      val intersectionPoint = List((b - a) / 2, (a + b) / 2)

      if (
        intersectionPoint.forall(coord => 0 < coord && coord < limit) &&
        sensors.forall(s =>
          md(
            (intersectionPoint.head, intersectionPoint.last),
            s
          ) > sensorToRadius(s)
        )
      ) {
        return 4000000L * intersectionPoint.head + intersectionPoint.last
      }
    }
    -1
  }

  private def coefficientsOfLinesOutsideSensorRadius(
      sensorToRadius: Map[(Int, Int), Int]
  ): (Set[Int], Set[Int]) = {
    val aCoeffs = mutable.Set.empty[Int]
    val bCoeffs = mutable.Set.empty[Int]

    sensorToRadius.foreach { case ((x, y), r) =>
      aCoeffs ++= List(y - x + r + 1, y - x - r - 1)
      bCoeffs ++= List(y + x + r + 1, y + x - r - 1)
    }

    (aCoeffs.toSet, bCoeffs.toSet)
  }

  val tinyInput = List("Sensor at x=8, y=7: closest beacon is at x=2, y=10")

  println(part1(testInput, 10))
  println(
    part1(
      scala.io.Source.fromResource("day15_in.txt").getLines().toList,
      2000000
    )
  )

  println(part2(testInput, 20))
  println(
    part2(
      scala.io.Source.fromResource("day15_in.txt").getLines().toList,
      4000000
    )
  )
}
