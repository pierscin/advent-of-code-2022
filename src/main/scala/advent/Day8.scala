package advent

object Day8 extends App {
  def normalize(ss: Seq[String]): Seq[Seq[Int]] = {
    ss.filter(_.nonEmpty).map(_.map(_.asDigit))
  }

  def solve(m: Seq[Seq[Int]]): Int = {
    val R = m.size
    val C = m.head.size

    val lToR = Array.ofDim[Int](R, C)
    val rToL = Array.ofDim[Int](R, C)
    val uToD = Array.ofDim[Int](R, C)
    val dToU = Array.ofDim[Int](R, C)

    var maxSoFar = 0

    for {
      r <- 0 until R
      c <- 0 until C
    } {
      if (c == 0) maxSoFar = 0

      lToR(r)(c) = maxSoFar
      maxSoFar = maxSoFar max m(r)(c)
    }

    for {
      r <- 0 until R
      c <- (0 until C).reverse
    } {
      if (c == C - 1) maxSoFar = 0

      rToL(r)(c) = maxSoFar
      maxSoFar = maxSoFar max m(r)(c)
    }

    for {
      c <- 0 until C
      r <- 0 until R
    } {
      if (r == 0) maxSoFar = 0

      uToD(r)(c) = maxSoFar
      maxSoFar = maxSoFar max m(r)(c)
    }

    for {
      c <- 0 until C
      r <- (0 until R).reverse
    } {
      if (r == R - 1) maxSoFar = 0

      dToU(r)(c) = maxSoFar
      maxSoFar = maxSoFar max m(r)(c)
    }

    var uncovered = 0

    for {
      r <- 1 until R - 1
      c <- 1 until C - 1
    } {
      val h = m(r)(c)
      if (Seq(lToR(r)(c), uToD(r)(c), rToL(r)(c), dToU(r)(c)).exists(_ < h)) {
        uncovered += 1
      }
    }

    uncovered + R + R + C + C - 4
  }

  val testInput = """30373
                    |25512
                    |65332
                    |33549
                    |35390
                    |""".stripMargin.split("\n")

  assert(solve(normalize(testInput)) == 21)
}
