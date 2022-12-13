package advent

import scala.collection.mutable
import cats.syntax.all._

object Day9 extends App {

  def solve(inputs: Seq[String], length: Int = 2): Int = {
    val visited = mutable.Set.empty[(Int, Int)]
    val rope = Array.fill(length)((0, 0))

    val dhToDt = Map(
      (2, 1) -> (1, 1),
      (1, 2) -> (1, 1),
      (2, 0) -> (1, 0),
      (2, -1) -> (1, -1),
      (1, -2) -> (1, -1),
      (0, -2) -> (0, -1),
      (-1, -2) -> (-1, -1),
      (-2, -1) -> (-1, -1),
      (-2, 0) -> (-1, 0),
      (-2, 1) -> (-1, 1),
      (-1, 2) -> (-1, 1),
      (0, 2) -> (0, 1),
      (2, 2) -> (1, 1),
      (-2, -2) -> (-1, -1),
      (-2, 2) -> (-1, 1),
      (2, -2) -> (1, -1)
    ).withDefaultValue((0,0))

    visited += ((0, 0))

    def diffTuples(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
      (a._1 - b._1, a._2 - b._2)
    }

    inputs
      .map(_.split(" "))
      .foreach { case Array(dir, n) =>
        (0 until n.toInt).foreach { _ =>
          {
            val dh =
              dir match {
                case "R" => (0, 1)
                case "L" => (0, -1)
                case "U" => (1, 0)
                case "D" => (-1, 0)
              }

            rope(0) = rope(0) |+| dh

            for (i <- 1 until length) {
              val diff = diffTuples(rope(i - 1), rope(i))
              rope(i) = rope(i) |+| dhToDt(diff)
            }

            visited += rope.last
          }
        }
      }

    visited.size
  }

  val testInput = """R 4
  |U 4
  |L 3
  |D 1
  |R 4
  |D 1
  |L 5
  |R 2""".stripMargin.split("\n")
//  println(solve(testInput))
//  println(solve(scala.io.Source.fromResource("day9_in.txt").getLines().toList, 10))
//  println(solve(testInput, 10))

  val test2 = """R 5
                |U 8
                |L 8
                |D 3
                |R 17
                |D 10
                |L 25
                |U 20""".stripMargin.split("\n")
  println(solve(test2, 10))
}
