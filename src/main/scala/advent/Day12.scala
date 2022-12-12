package advent

import scala.collection.mutable

object Day12 extends App {

  def solve(m: Array[Array[Char]], s: (Int, Int), e: (Int, Int)): Int = {
    m(s._1)(s._2) = 'a'
    m(e._1)(e._2) = 'z'

    val neighs = List((0, 1), (0, -1), (1, 0), (-1, 0))
    val visited = mutable.Set.empty[(Int, Int)]
    val R = m.length
    val C = m.head.length

    val q = mutable.ArrayDeque.empty[(Int, Int)]

    q += s
    var steps = 0

    while (q.nonEmpty) {
      var onThisLevel = q.length

      while (onThisLevel > 0) {
        val (r, c) = q.removeHead()

        if (!visited.contains((r, c))) {
          neighs.foreach {
            case (dr, dc) =>
              if (r + dr < R &&
                r + dr >= 0 &&
                c + dc < C &&
                c + dc >= 0 &&
                m(r + dr)(c + dc) - m(r)(c) <= 1) {

                if (((r + dr),(c + dc)) == e)
                  return steps + 1
                else
                  q += ((r + dr, c + dc))
              }
          }
        }

        visited += ((r, c))

        onThisLevel -= 1
      }

      steps += 1
    }

    Int.MaxValue
  }

  val testInput = """Sabqponm
                    |abcryxxl
                    |accszExk
                    |acctuvwj
                    |abdefghi""".stripMargin.split("\n")


  def find2d(m: Array[Array[Char]], x: Char): (Int, Int) = {
    for {
      r <- m.indices
      c <- m.head.indices
    } {
      if (m(r)(c) == x) return (r, c)
    }

    throw new IllegalArgumentException
  }

  val M = testInput.map(_.toCharArray)
  val e = find2d(M, 'E')
  val s = find2d(M, 'S')
  println(solve(M, s, e))

  //part two
//  println(findAllIndicesOf(M, 'a').map{ case (r, c) => {
//    solve(M, (r, c), e)
//  }}.filter(_ != Int.MaxValue).min)

  def findAllIndicesOf(m: Array[Array[Char]], x: Char): List[(Int, Int)] = {
    val res = mutable.ArrayBuffer.empty[(Int, Int)]

    for {
      r <- m.indices
      c <- m.head.indices
    } {
      if (m(r)(c) == x) res += ((r, c))
    }

    res.toList
  }
}
