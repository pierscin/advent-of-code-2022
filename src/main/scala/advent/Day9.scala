package advent

import scala.collection.mutable
import cats.syntax.all._

object Day9 extends App {

  def solve(inputs: Seq[String]): Int = {
    val visited = mutable.Set.empty[(Int, Int)]

    var hCoords = (0, 0)
    var tCoords = (0, 0)
    var prevH = (0, 0)

    visited += ((0, 0))

    def absDiffTuples(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
      ((a._1 - b._1).abs, (a._2 - b._2).abs)
    }

    def shouldDoOneStep(): Boolean = {
      absDiffTuples(hCoords, tCoords) == (0, 2) || absDiffTuples(
        hCoords,
        tCoords
      ) == (2, 0)
    }

    def shouldDoDiagonalStep(): Boolean = {
      absDiffTuples(hCoords, tCoords) == (1, 2) || absDiffTuples(
        hCoords,
        tCoords
      ) == (2, 1)
    }

    inputs
      .map(_.split(" "))
      .foreach { case Array(dir, n) =>
        (0 until n.toInt).foreach { _ =>
          {
            val dh = dir match {
              case "R" => (0, 1)
              case "L" => (0, -1)
              case "U" => (1, 0)
              case "D" => (-1, 0)
            }

            prevH = hCoords
            hCoords = hCoords |+| dh

            if (shouldDoOneStep() || shouldDoDiagonalStep()) {
              tCoords = prevH
              visited += prevH
            }
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
  println(solve(testInput))
}
