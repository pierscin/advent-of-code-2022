package advent

object Day13 extends App {
  def solve(a: String, b: String): Boolean = {
    (a.head, b.head) match {
      case (ac, bc) if ac == bc => solve(a.tail, b.tail)
      case (']', _) => true
      case (_, ']') => false
      case ('[', digit) => solve(a.tail, digit + "]" + b.tail)
      case (digit, '[') => solve(digit + "]" + a.tail, b.tail)
      case (digitA, digitB) => digitA < digitB
    }
  }

  private def replaceTens(s: String) = {
    s.replace("10", ('9' + 1).toChar.toString) // There are '10' in input... Has to be something bigger than '9'
  }

  def part1(input: Seq[String]) = {
    var res = 0

    for (i <- 0 until(input.size, 3)) {
      if (solve(replaceTens(input(i)), replaceTens(input(i + 1)))) {
        res += (i / 3) + 1
      }
    }

    res
  }

  def part2(input: Seq[String]) = {
    val key1 = "[[2]]"
    val key2 = "[[6]]"

    val withDecodingKeys = key1 :: key2 :: input.filter(_.nonEmpty).toList
    val withoutTens = withDecodingKeys.map(replaceTens)

    val sorted = withoutTens.sortWith(solve)

    (sorted.indexOf(key1) + 1) * (sorted.indexOf(key2) + 1)
  }

  val testInput =
    """[1,1,3,1,1]
      |[1,1,5,1,1]
      |
      |[[1],[2,3,4]]
      |[[1],4]
      |
      |[9]
      |[[8,7,6]]
      |
      |[[4,4],4,4]
      |[[4,4],4,4,4]
      |
      |[7,7,7,7]
      |[7,7,7]
      |
      |[]
      |[3]
      |
      |[[[]]]
      |[[]]
      |
      |[1,[2,[3,[4,[5,6,7]]]],8,9]
      |[1,[2,[3,[4,[5,6,0]]]],8,9]
      |""".stripMargin.split("\n")

  println(part2(testInput))
  println(part2(scala.io.Source.fromResource("day13_in.txt").getLines().toList))
}
