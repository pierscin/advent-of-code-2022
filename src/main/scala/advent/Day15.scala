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

  def solve(input: Seq[String], row: Int) = {
    val beacons = mutable.ArrayBuffer.empty[(Int, Int)]
    val sensors = mutable.ArrayBuffer.empty[(Int, Int)]

    input.map(parseLine).map { case sensor :: beacon :: Nil =>
      beacons += beacon
      sensors += sensor
    }

    val all = (beacons.toSet ++ sensors.toSet)

    val M = mutable.Map.empty[Int, Set[Int]].withDefaultValue(Set.empty[Int])

    (beacons zip sensors).foreach { case (b, s) =>
      val manDistance = diffTuples(s, b)

      val d = manDistance._1.abs + manDistance._2.abs

      for {
        r <- -d to d if s._2 + r == row
        x = d - r.abs
        c <- -x to x
        fc = s._1 + c
        fr = s._2 + r
      } {
        if (!all.contains((fc, fr))) {
          M(fr) = M(fr) + fc
        }
      }
    }

    M(row).size
  }

  val tinyInput = List("Sensor at x=8, y=7: closest beacon is at x=2, y=10")

  println(solve(testInput, 10))

  println(
    solve(
      scala.io.Source.fromResource("day15_in.txt").getLines().toList,
      2000000
    )
  )
}
